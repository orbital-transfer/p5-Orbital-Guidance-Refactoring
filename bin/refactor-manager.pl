#!/usr/bin/env perl
# PODNAME: refactor-manager
# ABSTRACT: Manage refactor

use FindBin;
use lib "$FindBin::Bin/../lib";

use utf8;
use Function::Parameters;
use Modern::Perl;

package RefactorManager {
	use Mu;
	use CLI::Osprey;

	with 'MooX::Role::Logger';

	use Term::ReadLine;
	use Term::Choose qw(choose);
	use Term::TablePrint qw( print_table );
	use Term::ANSIColor qw(colorstrip colored);
	use Text::Table;
	use File::chdir;

	use Text::WordDiff;

	use Params::Util qw(_CLASS);

	use YAML::XS;

	use match::smart qw(match);

	use Orbital::Payload::Tool::GitGot;
	use Orbital::Payload::VCS::Git;
	use Capture::Tiny qw(capture_stdout);
	use Path::Tiny;
	use Try::Tiny;
	use List::AllUtils qw(first all);

	option gitgot_tags => (
		is => 'ro',
		required => 1,
		format => 's',
	);

	option refactor_data_path => (
		is => 'ro',
		required => 1,
		format => 's',
	);

	lazy base_branch_name => method() {
		return 'master';
	};

	lazy refactor_branch_name => method() {
		path($self->refactor_data_path)->basename =~ s/\..*$//r;
	};

	lazy my_readline => method() {
		my $term = Term::ReadLine->new(ref $self);
		die "Readline does not have preput"
			unless exists $term->Features->{preput};
		$term;
	};

	has refactor_data => (
		is => 'rw',
		default => sub { +{} },
	);

	method read_data() {
		return unless -f $self->refactor_data_path;
		my $data = YAML::XS::LoadFile( $self->refactor_data_path );

		$self->refactor_data( $data );
	}

	method write_data() {
		YAML::XS::DumpFile( $self->refactor_data_path, $self->refactor_data );
	}

	lazy gitgot => method() {
		Orbital::Payload::Tool::GitGot->new;
	};

	lazy repositories_with_tag => method() {
		my $repos = $self->gitgot->data;
		my @tags_re = map { qr/$_/ } split /,/, $self->gitgot_tags;
		[ grep {
			my $tags = $_->repo_tags;
			all { match( $tags, $_ ) } @tags_re;
		} @$repos ];
	};

	method check_if_repos_dirty() {
		my $repos = $self->repositories_with_tag;

		for my $repo (@$repos) {
			my $git = Orbital::Payload::VCS::Git->new(
				directory => $repo->repo_path,
			);

			die "Repo @{[ $repo->repo_name ]} is dirty\n"
				if $git->_git_wrapper->status->is_dirty;
		}
	}

	method setup_refactor_branch() {
		my $repos = $self->repositories_with_tag;

		for my $repo (@$repos) {
			my $git = Orbital::Payload::VCS::Git->new(
				directory => $repo->repo_path,
			);

			$self->_logger->info("Working on repo: @{[ $repo->repo_path ]}");

			# force reset from master branch
			$git->_git_wrapper->checkout( { 'B', $self->refactor_branch_name }, $self->base_branch_name);
		}
	}

	method execute_actions() {
		$self->check_if_repos_dirty;
		$self->setup_refactor_branch;

		my $data = $self->refactor_data;

		my @packages = sort { $a cmp $b } keys %{ $data->{changes} };

		my @repo_action_cbs;
		my @name_action_cbs;

		my $refactor_message = "Refactor @{[ $self->refactor_branch_name ]}";

		my @packages_with_name_changes;
		for my $package ( @packages ) {
			my $actions = $data->{changes}{$package};
			next unless keys %$actions;

			my $texts = $self->get_package_texts( $package );

			push @repo_action_cbs, sub {
				# first move repos
				if( exists $actions->{repo} ) {
					my $action = $actions->{repo};
					my $text = colorstrip($texts->{repo_long});
					my $repo = $self->get_repo_from_path($data->{packages}{$package}{repo}{path});
					my $files = $self->get_files_for_package( $repo, $package );
					if( $action->{action} eq 'CHANGE' ) {
						say "Moving $package from repo: $text";
						$self->move_files(
							$files,
							$self->get_repo_from_path( $action->{from} ),
							$self->get_repo_from_path( $action->{to} ),
							[
								"$refactor_message [move]",
								"- Moving $package from repos: $text"
							]
						);
					} elsif( $action->{action} eq 'KEEP' ) {
						say "Keeping $package in same repo: $text";
						# do nothing
					} else {
						say STDERR "Do not know what to do with repo action: $action->{action}";
					}
				}
			};

			push @name_action_cbs, sub {
				# then move name
				if( exists $actions->{name} ) {
					my $action = $actions->{name};
					my $text = colorstrip($texts->{name_long});
					if( $action->{action} eq 'CHANGE' ) {
						say "Renaming $package: $text";
						$self->rename_class(
							$action->{from},
							$action->{to},
							[
								"$refactor_message [rename]",
								"- Renaming package: $text"
							]
						);
						push @packages_with_name_changes, $package;
					} elsif( $action->{action} eq 'KEEP' ) {
						say "Keeping $package name: $text";
						# do nothing
					} else {
						say STDERR "Do not know what to do with name action: $action->{action}";
					}
				}
			};
		}

		$_->() for (@repo_action_cbs, @name_action_cbs);

		$self->check_if_repos_dirty;
		my $repos = $self->repositories_with_tag;

		for my $repo (@$repos) {
			my $git = Orbital::Payload::VCS::Git->new(
				directory => $repo->repo_path,
			);

			if( ! $self->does_git_repo_have_commits($git) ) {
				$git->_git_wrapper->checkout($self->base_branch_name);
				$git->_git_wrapper->branch(qw(--delete --force), $self->refactor_branch_name);
			} else {
				{
					#local @ENV{qw(GIT_SEQUENCE_EDITOR GIT_EDITOR)};
					local $ENV{GIT_SEQUENCE_EDITOR} = ":";
					local $ENV{GIT_EDITOR} = ":";
					local $CWD = $git->_git_wrapper->dir;
					# Can not use Git::Wrapper because it sets GIT_EDITOR itself
					system(
						$git->_git_wrapper->git,
						qw(rebase),
						qw(--autosquash --interactive),
						$self->base_branch_name
					) == 0 or die "error: git rebase";
				}
			}
		}

		my %matches;
		@packages_with_name_changes = sort { $a cmp $b } @packages_with_name_changes;
		for my $repo (@$repos) {
			my $git = Orbital::Payload::VCS::Git->new(
				directory => $repo->repo_path,
			);
			for my $package ( @packages_with_name_changes ) {
				my @grep = try { $git->_git_wrapper->grep( $package ) };
				push @{ $matches{$repo->repo_name} }, $package if @grep;
			}
		}
		if( %matches ) {
			say "Double check for packages:";
			my @repos = sort keys %matches;
			for my $repo (@repos) {
				say "$repo: ", join(" ", @{ $matches{$repo} } );
			}
		}
	}

	method does_git_repo_have_commits($git) {
		return ($git->_git_wrapper->rev_parse($self->base_branch_name))[0]
			ne ($git->_git_wrapper->rev_parse($self->refactor_branch_name))[0];
	}

	method get_repo_from_path($path) {
		first {
			$_->repo_path eq $path;
		} @{ $self->gitgot->data }
	}

	method get_files_for_package( $repo, $package ) {
		[ grep { -f } @{ $self->get_all_possible_package_files($repo, $package) } ];
	}

	method get_all_possible_package_files( $repo, $package ) {
		return [
			$self->get_package_pm_file( $repo, $package ),
			$self->get_package_test_file( $repo, $package ),
		];
	}

	method move_files($files, $from_repo, $to_repo, $message) {
		my $from_git = Orbital::Payload::VCS::Git->new(
			directory => $from_repo->repo_path,
		);
		my $to_git = Orbital::Payload::VCS::Git->new(
			directory => $to_repo->repo_path,
		);

		for my $file (@$files) {
			my $destination = $file
				->relative($from_repo->repo_path)
				->absolute($to_repo->repo_path);
			die "Destination file already exists: $destination\n"
				if -f $destination;
			$destination->parent->mkpath;
			$file->move( $destination );
		}
		for my $git ($from_git, $to_git) {
			$self->git_commit_message($git, $message);
		}
	}

	method rename_class( $from_package, $to_package, $message ) {
		my $repos = $self->repositories_with_tag;

		for my $repo (@$repos) {
			my $git = Orbital::Payload::VCS::Git->new(
				directory => $repo->repo_path,
			);

			{
				local $CWD = $repo->repo_path;
				system( qw(prt rename_class), $from_package, $to_package );
				# note: requires File::CodeSearch
				# this is to replace in roles (with qw(...))
				system(
					$^X,

					## replace prompt for: use IO::Prompt qw/prompt/;
					## to automatically answer yes
					#q|-M5;*prompt = sub { "y" }|,

					qw(-S cs),
					qw(--yes), # answer yes
					qw(--path bin:lib:script:t),

					# note: maybe have negative lookbehind for ':' ?
					qq{\\b$from_package\\b(?!:)},
					qw(-r), $to_package
				);
			}

			my $from_files = $self->get_all_possible_package_files( $repo, $from_package );
			my $to_files = $self->get_all_possible_package_files( $repo, $to_package );
			for my $file_idx (0..@$from_files-1) {
				if( -f $from_files->[$file_idx] ) {
					die "Destination file already exists: @{[ $to_files->[$file_idx] ]}\n"
						if -f $to_files->[$file_idx];
					$to_files->[$file_idx]->parent->mkpath;
					$from_files->[$file_idx]
						->move($to_files->[$file_idx]);
				}
			}
			$self->git_commit_message($git, $message);
		}
	}

	method git_commit_message($git, $message) {
		state $repo_message_needs_squashing;

		my @messages = ( @$message );
		if( $git->_git_wrapper->status->is_dirty ) {
			$git->_git_wrapper->add('.');
			#my $_info = { has_commits => $has_commits, messages => \@messages }; use DDP; p $_info;
			my $first_message = shift @messages;

			my $do_squash = exists $repo_message_needs_squashing->{$git->directory}{$first_message};
			$git->_git_wrapper->commit(
				( $do_squash
					? ( { qw(message), "squash! $first_message" } )
					: ( { qw(message), $first_message } ) ),
				(
					map {
						{ qw(message), $_ }
					} @messages
				)
			);
			$repo_message_needs_squashing->{$git->directory}{$first_message} = 1;
		}

	}

	method top_level() {
		state $last_action = 0;
		my @actions = (
			{
				text => 'Select repos',
				cb => \&select_repos,
			},
			{
				text => 'Show actions',
				cb => \&show_actions,
			},
			{
				text => 'Execute actions',
				cb => \&execute_actions,
			}
		);
		my $index = choose(
			[ map { $_->{text} } @actions ],
			{
				prompt => 'Select action',
				index => 1,
				color => 2,
				default => $last_action,
			},
		);
		if( ! defined $index ) {
			$self->_run_loop(0);
			return;
		}

		$last_action = $index;
		my $cb = $actions[$index]{cb};
		$self->$cb;
	}

	method show_actions() {
		my $data = $self->refactor_data;
		my @packages = keys %{ $data->{changes} };
		my @table_packages;
		for my $package ( @packages ) {
			my $actions = $data->{changes}{$package};
			if( keys %$actions ) {
				push @table_packages, $package;
			}
		}
		print_table(
			[
				map { [ $_ ] }
				$self->get_table_for_packages( [ sort { $a cmp $b } @table_packages ],
					sub {
						sort {
							for my $n (reverse 0..@$a-1) {
								my $cmp = $a->[$n] cmp $b->[$n];
								return $cmp if $cmp != 0;
							}
						} @_;
					}
				)->table
			],
			{
				color => 2,
			}
		);
	}

	method select_from_repos($repos, $args = {}) {
		my @repo_names = map {
			$_->repo_name
				=~ s{^([^/]+)/([^/]+)$}{colored(['green'], $1).'/'.colored(['blue'], $2)}egr;
		} @$repos;
		my $repo_idx = choose(
			\@repo_names,
			{
				prompt => 'Select repository to process',
				index => 1,
				color => 2,
				%$args,
			},
		);

		if( ! defined $repo_idx ) {
			return;
		}

		my $repo = $repos->[$repo_idx];
		return ($repo, $repo_idx);
	}

	method select_repos() {
		state $last_repo = 0;

		while(1) {
			$self->check_if_repos_dirty;

			my $repos = $self->repositories_with_tag;

			my $args = { default => $last_repo };
			my ($repo, $repo_idx) = $self->select_from_repos( $repos, $args );
			if( $repo ) {
				$last_repo = $repo_idx;
				$self->process_repo( $repo );
			} else {
				$last_repo = 0;
				return;
			}
		}
	}

	method get_packages($repo) {
		my $re = qr/^package\s+([^;]+)/m;
		my ($ack_output) = capture_stdout {
			local $CWD = $repo->repo_path;
			system(
				qw(ack --nogroup),
				$re
			);
		};
		my @matches = map { [ split /:/, $_, 3 ] } split(/\n/, $ack_output);

		my @packages;
		for my $match (@matches) {
			my ($package) = $match->[2] =~ $re;
			my $file_to_package = $match->[0]
				=~ s,^lib/,,gr
				=~ s,\Q.pm\E$,,gr
				=~ s,/,::,gr;
			if( $file_to_package eq $package  ) {
				push @packages, $package;
			}
		}

		\@packages;
	}

	method get_package_texts( $package_name ) {
		my $data = $self->refactor_data;

		my $change_count = 0;
		my $keep_count = 0;

		my $texts = {};
		if( exists $data->{changes}{$package_name}{name} ) {
			my $change = $data->{changes}{$package_name}{name};
			$texts->{name} = 'TODO';
			if( exists $change->{action} ) {
				if( $change->{action} eq 'KEEP' ) {
					$texts->{name} = colored(['green'], 'k()');
					$texts->{name_long} = $package_name;
					$keep_count++;
				} elsif( $change->{action} eq 'CHANGE' ) {
					$texts->{name} =
						colored(['magenta'], "c(")
						. word_diff(\($change->{from}), \($change->{to}), { STYLE => 'MyWordDiffANSI' })
						. colored(['magenta'], ")")
						;
					$texts->{name_long} =
						join colored([ 'bold' ], " ⇒ "),
						$change->{from}, $change->{to};
					$change_count++;
				}
			}
		} else {
			$texts->{name} = undef;
		}


		if( exists $data->{changes}{$package_name}{repo} ) {
			my $change = $data->{changes}{$package_name}{repo};
			$texts->{repo} = 'TODO';
			if( exists $change->{action} ) {
				if( $change->{action} eq 'KEEP' ) {
					$texts->{repo} = colored(['green'], 'k()');
					$texts->{repo_long} = path($data->{packages}{$package_name}{repo}{path})->basename;
					$keep_count++;
				} elsif( $change->{action} eq 'CHANGE' ) {
					$texts->{repo} =
						colored(['magenta'], "c(")
						. word_diff(\("".path($change->{from})->basename), \("".path($change->{to})->basename), { STYLE => 'MyWordDiffANSI' })
						. colored(['magenta'], ")")
						;
					$texts->{repo_long} =
						join colored([ 'bold' ], " ⇒ "),
						path($change->{from})->basename,
						path($change->{to})->basename;
					$change_count++;
				}
			}
		} else {
			$texts->{repo} = undef;
		}

		my $attr = '';
		if ( $change_count == 1 )    { $attr = 'yellow' }
		elsif ( $change_count == 2 ) { $attr = 'cyan' }
		else                         { $attr = $keep_count == 2 ? 'green' : ''  }

		$texts->{package} = colored( [ $attr, 'underline' ], $package_name );

		return $texts;
	}

	method get_table_for_packages( $packages, $cb = sub { return @_ } ) {
			my $table = Text::Table->new(
				{ title => 'Package', },
				{ title => 'Name Action', },
				{ title => 'Repo Action', },
			);

			$table->load(
				$cb->(
				map {
					my $package_name = $_;

					my $texts = $self->get_package_texts( $package_name );
					[
						$texts->{package},
						$texts->{name} // 'none',
						$texts->{repo} // 'none',
					],
				} @$packages
				)
			);
	}

	method process_repo($repo) {
		state $last_package = 0;

		while(1) {
			my $data = $self->refactor_data;
			my $packages = $self->get_packages( $repo );

			for my $package (@$packages) {
				$data->{packages}{$package}{repo}{path} = $repo->repo_path;
			}

			my $table = $self->get_table_for_packages( $packages );
			my @lines = $table->body;

			my $package_idx = choose(
				\@lines,
				{
					prompt => "Select package to process in @{[ $repo->repo_name ]}",
					index => 1,
					color => 2,
					default => $last_package,
				},
			);
			if( !defined $package_idx ) {
				$last_package = 0;
				return;
			}
			$last_package = $package_idx;
			$self->process_package( $repo, $packages->[$package_idx] );
		}
	}

	method get_package_pm_file( $repo, $package ) {
		path("lib/" . ( $package =~ s,::,/,gr ) . '.pm')
			->absolute( $repo->repo_path );
	}

	method get_package_test_file( $repo, $package ) {
		path("t/" . ( $package =~ s,::,/,gr ) . '.t')
			->absolute( $repo->repo_path );
	}

	method process_package( $repo, $package ) {
		my $package_data = $self->refactor_data->{changes}{$package};
		my $package_pm_file = $self->get_package_pm_file( $repo, $package );

		my @actions = (
			{
				text => 'Keep package name',
				cb => sub {
					$package_data->{name} = {
						action => 'KEEP',
					};
				},
			},
			{
				text => 'Keep repo',
				cb => sub {
					$package_data->{repo} = {
						action => 'KEEP',
					};
				},
			},
			{
				text => 'Change package name',
				cb => sub {
					my $preput = $package;
					if( exists $package_data->{name}{action}
						&& $package_data->{name}{action} eq 'CHANGE' ) {
						$preput = $package_data->{name}{to};
					}
					my $new_package = $self->my_readline->readline('New package name: ', $preput);
					if( defined $new_package && _CLASS($new_package) ) {
						if( $new_package ne $package ) {
							$package_data->{name} = {
								action => 'CHANGE',
								from => $package,
								to => $new_package,
							};
						} else {
							$package_data->{name} = {
								action => 'KEEP',
							};
						}
					}
				},
			},
			{
				text => 'Change repo',
				cb => sub {
					my $repos = $self->repositories_with_tag;

					my ($new_repo, $new_repo_idx) = $self->select_from_repos( $repos );
					if( $new_repo ) {
						if( $new_repo->repo_path ne $repo->repo_path ) {
							$package_data->{repo} = {
								action => 'CHANGE',
								from => $repo->repo_path,
								to => $new_repo->repo_path,
							};
						} else {
							$package_data->{repo} = {
								action => 'KEEP',
							};
						}
					}
				},
			},
			{
				text => 'Show changes',
				cb => sub {
					choose(
						[ 'Press ENTER to continue' ],
						{
							prompt => join("\n",
								@{ $self->get_package_texts( $package ) }{ qw(name_long repo_long) }
							),
							color => 2,
						}
					);
				},
			},
			{
				text => 'Show head of contents',
				cb => sub {
					choose(
						[ 'Press ENTER to continue' ],
						{ prompt => join("", $package_pm_file->lines({ count => 20 })) }
					);
				},
			},
			{
				text => 'Show contents in pager',
				cb => sub {
					die "\$PAGER not set" unless $ENV{PAGER};
					system(
						$ENV{PAGER},
						"$package_pm_file"
					);
				},
			},
			{
				text => 'Show contents in editor',
				cb => sub {
					die "\$EDITOR not set" unless $ENV{EDITOR};
					system(
						$ENV{EDITOR},
						"$package_pm_file"
					);
				},
			},
		);

		while(1) {
			my $table = $self->get_table_for_packages( [ $package ] );

			my $index = choose(
				[ map { $_->{text} } @actions ],
				{
					prompt => <<~EOF,
					Select action for $package in @{[ $repo->repo_name ]}:
					$table
					EOF
					index => 1,
					color => 2,
				},
			);
			if( ! defined $index ) {
				return;
			}

			my $cb = $actions[$index]{cb};
			$self->$cb;
		}
	}

	has _run_loop => ( is => 'rw' );

	use Log::Any::Adapter ('Stderr');
	method run() {
		binmode STDOUT, ':encoding(UTF-8)';
		$self->read_data;
		$self->_run_loop(1);
		while($self->_run_loop) {
			$self->top_level;
		}

		$self->save;
	}

	method save() {
		say "That's all folks!";
		$self->write_data;
	}
}

package MyWordDiffANSI {
	use strict;
	use Term::ANSIColor qw(:constants);
	use vars qw($VERSION @ISA);

	# Term::ANSIColor doesn't support STRIKETHROUGH, so we'll do it ourselves.
	use constant STRIKETHROUGH => "\e[9m";

	@ISA = qw(Text::WordDiff::Base);

	sub same_items {
		shift;
		return join '', @_;
	}

	sub delete_items {
		shift;
		return join '', BOLD, RED, STRIKETHROUGH, "[-", @_, "-]", RESET;
	}

	sub insert_items {
		shift;
		return join '', BOLD, GREEN, UNDERLINE, "{+", @_, "+}", RESET;
	}

	1;
}

sub main {
	RefactorManager->new_with_options->run;
}

main;
