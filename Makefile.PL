use strict;
use warnings;

use ExtUtils::MakeMaker;

WriteMakefile(
  NAME         => 'Mojolicious::Plugin::Start',
  VERSION_FROM => 'lib/Mojolicious/Plugin/Start.pm',
  AUTHOR       => 'A Good Programmer <nospam@cpan.org>',
  PREREQ_PM    => {'Mojolicious' => '9.38'},
  test         => {TESTS => 't/*.t'}
);
