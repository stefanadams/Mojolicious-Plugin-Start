package Mojolicious::Plugin::Start;
use Mojo::Base 'Mojolicious::Plugin', -signatures;

use 5.036;
use builtin qw(blessed);
use Mojo::Collection qw(c);
use Mojo::File qw();
use Mojo::IOLoop;
use Mojo::Util qw(camelize encode sha1_sum trim);
use Term::ANSIColor qw(colored);
use Time::HiRes qw(time);

our $VERSION = '0.01';

has level => undef;
has path => undef;
has stderr => 0;
has start_time => time;
has user => sub { sub ($c) {} };

sub register ($self, $app, $conf) {
  # Configure logging
  $app->helper('log'       => \&_log);
  $app->helper('paths.log' => sub { Mojo::File::path($ENV{LOGS_DIRECTORY} || $conf->{logs} || $app->home->child('log')) });
  $self->_log_config($app, $conf);

  # Exclude author commands in production mode
  $app->commands->namespaces([grep { !/::Author$/ } $app->commands->namespaces->@*]) if $app->mode eq 'production';
  push @{$app->commands->namespaces}, join '::', camelize($app->moniker), 'Command';

  # Switch to installable home directory
  my $app_home = Mojo::Home->new($app->home);
  $app->home(Mojo::Home->new($ENV{MOJO_DATA})) if $ENV{MOJO_DATA};

  # Switch to application "public" directory
  $app->static->paths->[0] = $app_home->child('public');

  # Switch to application "templates" directory
  $app->renderer->paths->[0] = $app_home->child('templates');

  $app->helper('no_favicon'   => \&_no_favicon);
  $app->helper('paths.app'    => sub { $app_home });
  $app->helper('paths.tmp'    => sub { Mojo::File::path($ENV{TMPDIR} || $conf->{tmp} || '/tmp') });
  $app->helper('paths.vartmp' => sub { Mojo::File::path($conf->{vartmp} || '/var/tmp') });
  $app->helper('paths.cache'  => sub { Mojo::File::path($ENV{CACHE_DIRECTORY} || $conf->{cache} || $app->home->child('cache')) });
  $app->helper('paths.run'    => sub { Mojo::File::path($ENV{RUNTIME_DIRECTORY} || $conf->{runtime} || $app->home->child('run')) });
  $app->helper('paths.state'  => sub { Mojo::File::path($ENV{STATE_DIRECTORY} || $conf->{state} || $app->home) });
  $app->helper('reply.close'  => \&_close);
  $app->helper('reply.ok' => sub ($c) { $c->rendered(200) });
  $app->helper('reply.time' => sub ($c) { $c->render(text => scalar localtime) });
  $app->helper('browser.signature' => sub ($c) {
    my $sig_components = join ' ', grep { $_ } $c->req->headers->user_agent, $c->req->headers->header('X-Real-IP') || $c->req->headers->header('X-Forwarded-For') || $c->tx->remote_address;
    my $sig = $c->cookie('browser_signature') || $c->stash('browser_signature');
    unless ($sig) {
      $sig = sha1_sum $sig_components;
      $c->cookie(browser_signature => $sig)->stash(browser_signature => $sig);
    }
    return ($sig, $sig_components);
  });
  $app->helper('update_paths' => sub ($c, @paths) {
    s/\$home/$c->app->home/e foreach grep { !blessed($_) && !ref} @paths;
    $_ = $app->home->child($_) foreach grep { blessed($_) && $_->isa('Mojo::File') && !$_->is_abs } @paths;
    return @paths;
  });

  # Enable capturing transactions if tx_dir exists (defaults to $home/log)
  $app->plugin('CaptureTX');

  # Indicate log path on server start
  $app->hook(before_server_start => sub ($server, $app) {
    my $log_time = _is_true($ENV{MOJO_LOG_TIME} || $conf->{log_time} || $app->config->{log_time});
    # Start the uptime clock
    $self->start_time and $app->log->info(sprintf 'Start logging %s.%s (%s)', $app->moniker, $app->mode, join('+', ($self->stderr?'stderr':()),  ($self->path?'file':()))||'n/a');
    $app->log->info(sprintf "Home/Data directory: %s", $app->home);
    $app->log->info(sprintf "App static path directory: %s", $app->static->paths->[0]);
    $app->log->info(sprintf "App template path directory: %s", $app->renderer->paths->[0]);
    $app->log->info(sprintf "Log directory: %s", $app->log->path || 'n/a');
  });

  # Immediately close connection on request error (Bad request start-line)
  # This is most likely from a port scan, but could be a malformed request
  $app->hook(around_dispatch => sub { _error(pop, 400) || shift->() });

  # if development_signatures doesn't exist, disable dev mode signature requirements
  # if development_signatures exists but is not defined, simply close the connection without displaying the browser signature
  # if development_signatures is defined, only allow signatures in the list or return the browser signature for adding the config file otherwise
  $app->hook(before_dispatch => sub ($c) {
    no warnings 'uninitialized';
    return if $c->tx->remote_address eq $c->tx->local_address;
    return unless $c->app->mode eq 'development' && exists $c->app->config->{development_signatures};
    return $c->reply->close unless defined $c->app->config->{development_signatures};
    my $dev_sigs = $c->app->config->{development_signatures} || [];
    my ($sig, $sig_components) = $c->browser->signature;
    return if grep { $_ eq $sig } @$dev_sigs;
    $c->render(text => $c->no_favicon.$sig);
    $c->rendered(200);
  });

  # Mojolicious::Routes L93 logs routing to callback routes, but for performance reasons does not log the route name
  $app->hook(around_action => sub ($next, $c, $action, $last) {
    if ($last && !$c->match->stack->[-1]->{action}) {
      $c->log->trace(sprintf 'Routing to a callback route named %s', $c->current_route);
    }
    $next->();
  }) if $app->mode eq 'development';

  # Log when not_found or exception templates are rendered
  $app->hook(before_render => sub ($c, $args) {
    my $user = ref $conf->{user} eq 'CODE' ? $conf->{user}->($c) : '';
    my $match = $c->match;
    my $stack = $match->stack->[-1];
    my $e = $c->stash->{exception} || $args->{exception};
    my $template = $args->{template} || '';
    if ($e && $args->{status} >= 500 && ($template eq 'exception' || $args->{text})) { # Make sure we are rendering the exception template
      my $cal = sprintf '%s%s', join('#', grep {$_} $stack->{controller}, $stack->{action}) || $match->endpoint->name.' callback', ($e->line->[0] ?  '#L'.$e->line->[0] : '');
      my @raptor = ($c->req->request_id, $args->{status}, $user, $c->req->url, $cal, $e);
      my $error_head = join ' ', map { "[$_]" } grep { $_ } @raptor[1..2], $cal;
      $c->log->error(sprintf '%s Raptor Exception shown for %s: %s', $error_head, $c->req->url, trim $e);
      $app->log->emit('raptor_exception', @raptor);
    }
    elsif ($template eq 'not_found') { # Make sure we are rendering the not_found template
      my @raptor = ($c->req->request_id, $args->{status}, $user, $c->req->url);
      $c->log->warn(sprintf '[%s] [%s] Raptor Not Found shown for %s', @raptor[1..$#raptor]);
      $app->log->emit('raptor_not_found', @raptor);
    }
  });

  $app->hook(after_render => sub ($c, $output, $format) {
    my ($sig, $sig_components) = $c->browser->signature;
    my $req_cookie = c($c->req->cookies->@*)->grep(sub{$_->name eq 'browser_signature' && $_->value =~ /$sig$/})->size;
    $c->log->info(sprintf '%sBrowser Signature %s Denied for %s', ($req_cookie ? '🍪 ' : ''), $sig, $sig_components) if $$output =~ /$sig$/;
  });

  return $self;
}

sub _close ($c, $status=503) {
  $c->rendered($status);
  Mojo::IOLoop->stream($c->tx->connection)->close;
}
sub _error ($c, $status) { $c->req->error ? _close($c, $status) : undef }

sub _no_favicon ($c) { qq{<link rel="icon" href="data:;base64,iVBORw0KGgo=">} }

sub _is_false ($value) { return undef unless defined $value; $value =~ /^\s*(0|n|no|off|false)\s*$/i }
sub _is_true  ($value) { return undef unless defined $value; $value =~ /^\s*(1|y|yes|on|true)\s*$/i }

sub _log { $_[0]->stash->{'mojo.log'} ||= $_[0]->app->log->context(map{"[$_]"} $_[0]->tx->remote_address, $_[0]->req->request_id) }
sub _log_config ($self, $app, $conf) {
  # It seems this needs to be set right up front and changing its value later seems to have no effect
  $app->log->color($ENV{MOJO_LOG_COLOR} // $conf->{log_color} // $app->config->{log_color} // 1);

  # Determine a place to store logs if dir exists, defaults to $home/log
  if (-d $app->paths->log) {
    $self->path(Mojo::File::path($app->paths->log)->child($app->mode . '.log'));
  }
  elsif (my $log_path = $conf->{log_path} || $app->config->{log_path}) {
    my $path = Mojo::File::path($log_path)->dirname if $log_path;
    $self->path($path) if -d $path;
  }
  $app->log->path($self->path->realpath) if $self->path;

  # Change the log level in the app configuration or via MOJO_LOG_LEVEL
  $self->level($ENV{MOJO_LOG_LEVEL} || $conf->{log_level} || $app->config->{log_level});
  $app->log->level($self->level) if $self->level;

  # Log to STDERR if configured, also enable a short time since server start option
  $self->stderr(_is_true($ENV{MOJO_LOG_STDERR} || $conf->{log_stderr} || $app->config->{log_stderr}));
  $app->log->on(message => sub ($log, $level, @lines) {
    my $log_time = _is_true($ENV{MOJO_LOG_TIME} || $conf->{log_time} || $app->config->{log_time});
    if ($self->stderr && ($log->path || defined $log_time)) {
      warn encode 'UTF-8', _is_true($log_time) ? $log->format->(time, $level, @lines) : sprintf("[%.4f] [%s] %s\n", time - $self->start_time, $level, join " ", @lines);
    }
  });
}

package Mojolicious::Plugin::CaptureTX;
use Mojo::Base 'Mojolicious::Plugin', -signatures;

use Mojo::File qw(path);
use Mojo::Home;

has tx_dir  => sub { Mojo::Home->new->detect->child('tx') };
has skip_cb => sub { sub {} };

sub register {
  my ($self, $app, $conf) = @_;

  my $path = path($conf->{path}) if $conf->{path};
  $self->tx_dir(path($conf->{path})) if $path;
  $app->log->debug(sprintf 'Create directory %s to capture transactions', $self->tx_dir) and return unless -d $self->tx_dir;
  $app->log->debug(sprintf 'Capturing Transactions in %s', $self->tx_dir);
  $self->skip_cb($conf->{skip_cb}) if $conf->{skip_cb};
  $app->hook(after_build_tx => sub { $self->_capture($_[1], $_[0]) });
  $app->helper(capture_tx => sub { $self->_capture($_[0]->app, $_[1]) });
}

# Capture real requests for replaying later
# $ nc localhost 3000 < tx_dir/abc123
sub _capture ($self, $app, $tx) {
  $tx->on(connection => sub ($tx, $connection) {
    my $asset = $self->{assets}->{$connection} //= Mojo::Asset::File->new(cleanup => 0, path => $self->tx_dir->child($connection));
    my $stream = Mojo::IOLoop->stream($connection);
    $stream->on(read => sub ($stream, $bytes) {
      return if $self->skip_cb->($app, $tx, $stream, $bytes);
      $app->log->debug(sprintf '[%s] got %d bytes', $connection, length($bytes));
      $asset->add_chunk($bytes);
    });
    $stream->on(close => sub ($tx) {
      $app->log->debug(sprintf '[%s] captured %d-byte tx', $connection, $asset->size);
      delete $self->{assets}->{$connection};
    });
  });
}

1;

=encoding utf8

=head1 NAME

Mojolicious::Plugin::Start - Mojolicious Plugin

=head1 SYNOPSIS

  # Mojolicious
  $self->plugin('Start');

  # Mojolicious::Lite
  plugin 'Start';

=head1 DESCRIPTION

L<Mojolicious::Plugin::Start> is a L<Mojolicious> plugin.

=head1 METHODS

L<Mojolicious::Plugin::Start> inherits all methods from
L<Mojolicious::Plugin> and implements the following new ones.

=head2 register

  $plugin->register(Mojolicious->new);

Register plugin in L<Mojolicious> application.

=head1 SEE ALSO

L<Mojolicious>, L<Mojolicious::Guides>, L<https://mojolicious.org>.

=cut
