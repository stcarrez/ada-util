--  Generated by utildgen.c from system includes
with Interfaces.C;
package Util.Systems.Constants is

   pragma Pure;

   --  Flags used when opening a file with open/creat.
   O_RDONLY                      : constant Interfaces.C.int := 8#000000#;
   O_WRONLY                      : constant Interfaces.C.int := 8#000001#;
   O_RDWR                        : constant Interfaces.C.int := 8#000002#;
   O_CREAT                       : constant Interfaces.C.int := 8#001000#;
   O_EXCL                        : constant Interfaces.C.int := 8#004000#;
   O_TRUNC                       : constant Interfaces.C.int := 8#002000#;
   O_APPEND                      : constant Interfaces.C.int := 8#000010#;
   O_CLOEXEC                     : constant Interfaces.C.int := 8#4000000#;
   O_SYNC                        : constant Interfaces.C.int := 8#000200#;
   O_DIRECT                      : constant Interfaces.C.int := 8#200000#;
   O_NONBLOCK                    : constant Interfaces.C.int := 8#000004#;

   --  Some error codes
   EPERM                         : constant := 1;
   ENOENT                        : constant := 2;
   EINTR                         : constant := 4;
   EIO                           : constant := 5;
   ENOEXEC                       : constant := 8;
   EBADF                         : constant := 9;
   EAGAIN                        : constant := 35;
   ENOMEM                        : constant := 12;
   EACCES                        : constant := 13;
   EFAULT                        : constant := 14;
   EBUSY                         : constant := 16;
   EEXIST                        : constant := 17;
   ENOTDIR                       : constant := 20;
   EISDIR                        : constant := 21;
   EINVAL                        : constant := 22;
   ENFILE                        : constant := 23;
   EMFILE                        : constant := 24;
   EFBIG                         : constant := 27;
   ENOSPC                        : constant := 28;
   EROFS                         : constant := 30;
   EPIPE                         : constant := 32;

   --  Flags used by fcntl
   F_SETFL                       : constant Interfaces.C.int := 4;
   F_GETFL                       : constant Interfaces.C.int := 3;
   FD_CLOEXEC                    : constant Interfaces.C.int := 1;

   --  Flags for termios

   --  Flags for termios c_cflag
   CSIZE   : constant := 8#001400#; --  Mask for character size
   CS5     : constant := 8#000000#;
   CS6     : constant := 8#000400#;
   CS7     : constant := 8#001000#;
   CS8     : constant := 8#001400#;
   CSTOPB  : constant := 8#002000#; --  Send 2 stop bits
   CREAD   : constant := 8#004000#; --  Enable receiver
   PARENB  : constant := 8#010000#; --  Event parity
   PARODD  : constant := 8#020000#; --  use odd parity
   HUPCL   : constant := 8#040000#; --  Hang up on last close
   CLOCAL  : constant := 8#100000#; --  Ignore modem status line

   --  Flags for termios c_lflag
   ISIG    : constant := 8#000200#; --  Enable signals
   ICANON  : constant := 8#000400#; --  Canonical input (erase and kill or suspend
   ECHO    : constant := 8#000010#; --  Enable echo
   ECHOE   : constant := 8#000002#; --  Echo ERASE as an error-correcting backspace
   ECHOK   : constant := 8#000004#; --  Echo KILL
   ECHONL  : constant := 8#000020#; --  Echo '\n'
   NOFLSH  : constant := 8#20000000000#; --  Disable flush after interrupt, quit
   TOSTOP  : constant := 8#20000000#; --
   ECHOCTL : constant := 8#000100#; --  Echo ctrl chars as char?

   --  Flags for termios c_iflag
   IGNBRK  : constant := 8#000001#; --  Ignore break condition
   BRKINT  : constant := 8#000002#; --  Signal interrupt on break
   IGNPAR  : constant := 8#000004#; --  Ignore characters with parity errors
   PARMRK  : constant := 8#000010#; --  Mark parity and framing errors
   INPCK   : constant := 8#000020#; --  Enable input parity check
   ISTRIP  : constant := 8#000040#; --  Strip 8th bit off characters
   INLCR   : constant := 8#000100#; --  Map NL to CR on input
   IGNCR   : constant := 8#000200#; --  Ignore CR
   ICRNL   : constant := 8#000400#; --  Map CR to NL on input
   IXON    : constant := 8#001000#; --  Enable start/stop output control
   IXANY   : constant := 8#004000#; --  Enable any character to restart output
   IXOFF   : constant := 8#002000#; --  Enable start/stop input control

   --  Flags for termios c_oflag
   OPOST   : constant := 8#000001#; --  Post-process output
   ONLCR   : constant := 8#000002#; --  Map NL to CR-NL on output
   OCRNL   : constant := 8#000020#; --  Map CR to NL on output
   ONOCR   : constant := 8#000040#; --  No CR output at column 0
   ONLRET  : constant := 8#000100#; --  NL performs CR function

   --  Flags for termios c_oflag
   TCSANOW   : constant := 0; --  make change immediately
   TCSADRAIN : constant := 1; --  drain output, then change
   TCSAFLUSH : constant := 2; --  drain output, flush input

   --  Flags used by dlopen
   RTLD_LAZY                     : constant Interfaces.C.int := 8#000001#;
   RTLD_NOW                      : constant Interfaces.C.int := 8#000002#;
   RTLD_NOLOAD                   : constant Interfaces.C.int := 8#020000#;
   RTLD_DEEPBIND                 : constant Interfaces.C.int := 8#000000#;
   RTLD_GLOBAL                   : constant Interfaces.C.int := 8#000400#;
   RTLD_LOCAL                    : constant Interfaces.C.int := 8#000000#;
   RTLD_NODELETE                 : constant Interfaces.C.int := 8#010000#;

   DLL_OPTIONS   : constant String := "-ldl";
   SYMBOL_PREFIX : constant String := "";

end Util.Systems.Constants;
