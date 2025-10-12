package Mojolicious::Command::start;
use Mojo::Base 'Mojolicious::Command', -signatures;

use Mojo::Util qw(extract_usage getopt);

has description => 'Smart application starter';
has usage       => sub { shift->extract_usage };

has perlbrew => sub {
  return () unless $ENV{PERLBREW_ROOT} && $ENV{PERLBREW_HOME} && $ENV{PERLBREW_PERL};
  return ['perlbrew', 'exec', '--with', join '@', grep {$_} $ENV{PERLBREW_PERL}, $ENV{PERLBREW_LIB}];
};

sub run ($self, @args) {
  $ENV{MOJO_LISTEN} = join ',', $self->app->config->{listen}->@* if $self->app->config->{listen};
  $ENV{MOJO_REVERSE_PROXY} = $self->app->config->{reverse_proxy} if $self->app->config->{reverse_proxy};
  $ENV{MOJO_LOG_LEVEL} = $self->app->config->{log_level} if $self->app->config->{log_level};
  $ENV{MOJO_LOG_SHORT} = $self->app->config->{log_short} if $self->app->config->{log_short};
  $ENV{MOJO_LOG_PATH} = $self->app->config->{log_path} if $self->app->config->{log_path};
  $ENV{MOJO_LOG_STDERR} = $self->app->config->{log_stderr} if $self->app->config->{log_stderr};
  die "missing configuration key 'start'\n" unless $self->app->config->{start};
  $self->app->log->debug(join ' ', @{$self->perlbrew}, $self->app->config->{start}, @args);
  _exec(@{$self->perlbrew}, $self->app->config->{start}, @args);
}

sub _exec {
  s/\$0/$0/g for @_;
  exec {$_[0]} $0, @_[1..$#_];
}

1;