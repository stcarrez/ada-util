/* Generate a package from system header definitions
--  Copyright (C) 2011, 2013, 2014, 2015, 2016 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
*/
#ifdef _MIPS_ARCH
# define _LARGEFILE64_SOURCE
#endif
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#ifdef HAVE_CURL
#include <curl/curl.h>
#endif
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

#ifndef RTLD_NOLOAD
# define RTLD_NOLOAD 0
#endif

#ifndef RTLD_DEEPBIND
# define RTLD_DEEPBIND 0
#endif

#ifndef RTLD_NODELETE
# define RTLD_NODELETE 0
#endif

#ifndef __x86_64__
# define NEED_PADDING 1
#else
# define NEED_PADDING 0
#endif

#ifndef S_IFLNK
# define S_IFLNK 0
#endif

#ifndef S_IFSOCK
# define S_IFSOCK 0
#endif

#ifndef S_ISUID
# define S_ISUID 0
#endif

#ifndef S_ISGID
# define S_ISGID 0
#endif

#define SIGNED   1
#define UNSIGNED 0

const char* get_type(int is_signed, int size)
{
    if (is_signed) {
        switch (size) {
        case 1:
            return "Interfaces.C.signed_char";
        case 2:
            return "Interfaces.C.short";
        case 4:
            if (sizeof(int) == 8) {
                return "Interfaces.C.long";
            } else {
                return "Interfaces.C.int";
            }

        case 8:
            return "Long_Long_Integer";
        default:
            return "<invalid>";
        }
    } else {
        switch (size) {
        case 1:
            return "Interfaces.C.unsigned_char";
        case 2:
            return "Interfaces.C.unsigned_short";
        case 4:
            if (sizeof(int) == 8) {
                return "Interfaces.C.unsigned_long";
            } else {
                return "Interfaces.C.unsigned";
            }
            
        case 8:
            return "Long_Long_Integer";
        default:
            return "<invalid>";
        }
    }
}

void gen_type(const char* name, int is_signed, int size)
{
    printf("   subtype %s is %s;\n", name, get_type(is_signed, size));
}

void gen_stat(void)
{
#ifdef __linux__
#ifdef _MIPS_ARCH
    printf("   STAT_NAME  : constant String := \"stat64\";\n");
    printf("   FSTAT_NAME : constant String := \"fstat64\";\n");
#else
    printf("   STAT_NAME  : constant String := \"stat\";\n");
    printf("   FSTAT_NAME : constant String := \"fstat\";\n");
#endif
    printf("   type Stat_Type is record\n");
    printf("      st_dev     : dev_t;\n");
#ifdef _MIPS_ARCH
    printf("      pad0_1     : Interfaces.C.unsigned_long;\n");
    printf("      pad0_2     : Interfaces.C.unsigned_long;\n");
    printf("      pad0_3     : Interfaces.C.unsigned_long;\n");
#else
    if (NEED_PADDING) {
        printf("      pad1       : Interfaces.C.unsigned_short;\n");
    }
#endif
    printf("      st_ino     : ino_t;\n");
#ifndef __x86_64__
    printf("      st_mode    : mode_t;\n");
    printf("      st_nlink   : nlink_t;\n");
#else
    printf("      st_nlink   : nlink_t;\n");
    printf("      st_mode    : mode_t;\n");
#endif
    printf("      st_uid     : uid_t;\n");
    printf("      st_gid     : gid_t;\n");
#ifdef __x86_64__
    printf("      pad1_0     : Interfaces.C.unsigned;\n");
#endif
    printf("      st_rdev    : dev_t;\n");
#ifdef _MIPS_ARCH
    printf("      pad1_0     : Interfaces.C.unsigned_long;\n");
    printf("      pad1_1     : Interfaces.C.unsigned_long;\n");
    printf("      pad1_2     : Interfaces.C.unsigned_long;\n");
#else
    if (NEED_PADDING) {
        printf("      pad2       : Interfaces.C.unsigned_short;\n");
    }
#endif
    printf("      st_size    : off_t;\n");
#ifndef _MIPS_ARCH
    printf("      st_blksize : blksize_t;\n");
    printf("      st_blocks  : blkcnt_t;\n");
#ifndef __x86_64__
    printf("      pad3_1     : Interfaces.C.unsigned_long;\n");
#endif
#endif
    printf("      st_atim    : Timespec;\n");
    printf("      st_mtim    : Timespec;\n");
    printf("      st_ctim    : Timespec;\n");
#ifdef _MIPS_ARCH
    printf("      st_blksize : blksize_t;\n");
    printf("      pad2       : Interfaces.C.unsigned_long;\n");
    printf("      st_blocks  : blkcnt_t;\n");
#else
    if (NEED_PADDING) {
        printf("      pad3       : Interfaces.C.unsigned_long;\n");
        printf("      pad4       : Interfaces.C.unsigned_long;\n");
    }
#endif
#ifdef __x86_64__
    printf("      pad5    : Interfaces.C.unsigned_long;\n");
    printf("      pad6    : Interfaces.C.unsigned_long;\n");
    printf("      pad7    : Interfaces.C.unsigned_long;\n");
#endif
    printf("   end record;\n");
    printf("   pragma Convention (C_Pass_By_Copy, Stat_Type);\n");
    printf("\n");
#elif defined(__DragonFly__)
    struct stat st;

    gen_type("uint16_t", UNSIGNED, sizeof(uint16_t));
    gen_type("u_int32_t", UNSIGNED, sizeof(u_int32_t));
    gen_type("int32_t", SIGNED, sizeof(int32_t));
    gen_type("int64_t", SIGNED, sizeof(int64_t));

    printf("   STAT_NAME  : constant String := \"stat\";\n");
    printf("   FSTAT_NAME : constant String := \"fstat\";\n");
    printf("   type Stat_Type is record\n");
    printf("      st_ino      : ino_t;\n");
    printf("      st_nlink    : nlink_t;\n");
    printf("      st_dev      : dev_t;\n");
    printf("      st_mode     : mode_t;\n");
    printf("      st_padding1 : uint16_t;\n");
    printf("      st_uid      : uid_t;\n");
    printf("      st_gid      : gid_t;\n");
    printf("      st_rdev     : dev_t;\n");
    printf("      st_atim     : Timespec;\n");
    printf("      st_mtim     : Timespec;\n");
    printf("      st_ctim     : Timespec;\n");
    printf("      st_size     : off_t;\n");
    printf("      st_blocks   : int64_t;\n");
    printf("      st_blksize  : u_int32_t;\n");
    printf("      st_flags    : u_int32_t;\n");
    printf("      st_gen      : u_int32_t;\n");
    printf("      st_lspare   : int32_t;\n");
    printf("      st_qspare1  : int64_t;\n");
    printf("      st_qspare2  : int64_t;\n");
    printf("   end record;\n");
    printf("   pragma Convention (C_Pass_By_Copy, Stat_Type);\n");
    printf("\n");
#elif defined(__FreeBSD__)
    struct stat st;

    gen_type("fflags_t", UNSIGNED, sizeof(fflags_t));

    printf("   STAT_NAME  : constant String := \"stat\";\n");
    printf("   FSTAT_NAME : constant String := \"fstat\";\n");
    printf("   type Stat_Type is record\n");
    printf("      st_dev      : dev_t;\n");
    printf("      st_ino      : ino_t;\n");
    printf("      st_mode     : mode_t;\n");
    printf("      st_nlink    : nlink_t;\n");
    printf("      st_uid      : uid_t;\n");
    printf("      st_gid      : gid_t;\n");
    printf("      st_rdev     : dev_t;\n");
    printf("      st_atim     : Timespec;\n");
    printf("      st_mtim     : Timespec;\n");
    printf("      st_ctim     : Timespec;\n");
    printf("      st_size     : off_t;\n");
    printf("      st_blocks   : blkcnt_t;\n");
    printf("      st_blksize  : blksize_t;\n");
    printf("      st_flags    : fflags_t;\n");
    printf("      st_gen      : %s;\n", get_type(UNSIGNED, sizeof(st.st_gen)));
    printf("      st_lspare   : %s;\n", get_type(UNSIGNED, sizeof(st.st_lspare)));
    printf("      st_birthtim : Timespec;\n");
    printf("      st_spare1   : %s;\n", get_type(UNSIGNED, sizeof(unsigned)));
    printf("      st_spare2   : %s;\n", get_type(UNSIGNED, sizeof(unsigned)));
    printf("   end record;\n");
    printf("   pragma Convention (C_Pass_By_Copy, Stat_Type);\n");
    printf("\n");
#elif defined(__NetBSD__)
    struct stat st;

    printf("   STAT_NAME  : constant String := \"__stat50\";\n");
    printf("   FSTAT_NAME : constant String := \"__fstat50\";\n");
    printf("   type Stat_Type is record\n");
    printf("      st_dev       : dev_t;\n");
    printf("      st_mode      : mode_t;\n");
    printf("      st_ino       : ino_t;\n");
    printf("      st_nlink     : nlink_t;\n");
    printf("      st_uid       : uid_t;\n");
    printf("      st_gid       : gid_t;\n");
    printf("      st_rdev      : dev_t;\n");
    printf("      st_atim      : Timespec;\n");
    printf("      st_mtim      : Timespec;\n");
    printf("      st_ctim      : Timespec;\n");
    printf("      st_birthtime : Timespec;\n");
    printf("      st_size      : off_t;\n");
    printf("      st_blocks    : blkcnt_t;\n");
    printf("      st_blksize   : blksize_t;\n");
    printf("      st_flags     : %s;\n", get_type(UNSIGNED, sizeof(st.st_gen)));
    printf("      st_gen       : %s;\n", get_type(UNSIGNED, sizeof(st.st_gen)));
    printf("      st_spare1    : %s;\n", get_type(UNSIGNED, sizeof(st.st_spare[0])));
    printf("      st_spare2    : %s;\n", get_type(UNSIGNED, sizeof(st.st_spare[1])));
    printf("   end record;\n");
    printf("   pragma Convention (C_Pass_By_Copy, Stat_Type);\n");
    printf("\n");
#elif defined(_WIN32)
    printf("   type Stat_Type is record\n");
    printf("      st_dev      : dev_t;\n");
    printf("      st_ino      : ino_t;\n");
    printf("      st_mode     : mode_t;\n");
    printf("      st_nlink    : nlink_t;\n");
    printf("      st_uid      : uid_t;\n");
    printf("      st_gid      : gid_t;\n");
    printf("      st_rdev     : dev_t;\n");
    printf("      st_size     : off_t;\n");
    printf("      st_atime    : Time_Type;\n");
    printf("      st_mtime    : Time_Type;\n");
    printf("      st_ctime    : Time_Type;\n");
    printf("   end record;\n");
    printf("   pragma Convention (C_Pass_By_Copy, Stat_Type);\n");
    printf("\n");
#endif
}

int main(int argc, char** argv)
{
  printf("--  Generated by utildgen.c from system includes\n");

  if (argc > 1 && strcmp(argv[1], "curl") == 0) {
    printf("private package Util.Http.Clients.Curl.Constants is\n");
#ifdef HAVE_CURL
    printf("\n");
    printf("   CURLOPT_URL            : constant Curl_Option := %d;\n", CURLOPT_URL);
    printf("   CURLOPT_READFUNCTION   : constant Curl_Option := %d;\n", CURLOPT_READFUNCTION);
    printf("   CURLOPT_WRITEUNCTION   : constant Curl_Option := %d;\n", CURLOPT_WRITEFUNCTION);
    printf("   CURLOPT_HTTPHEADER     : constant Curl_Option := %d;\n", CURLOPT_HTTPHEADER);
    printf("   CURLOPT_INTERFACE      : constant Curl_Option := %d;\n", CURLOPT_INTERFACE);
    printf("   CURLOPT_USERPWD        : constant Curl_Option := %d;\n", CURLOPT_USERPWD);
    printf("   CURLOPT_HTTPAUTH       : constant Curl_Option := %d;\n", CURLOPT_HTTPAUTH);
    printf("   CURLOPT_MAXFILESIZE    : constant Curl_Option := %d;\n", CURLOPT_MAXFILESIZE);
    printf("   CURLOPT_WRITEDATA      : constant Curl_Option := %d;\n", CURLOPT_WRITEDATA);
    printf("   CURLOPT_HEADER         : constant Curl_Option := %d;\n", CURLOPT_HEADER);
    printf("   CURLOPT_POSTFIELDS     : constant Curl_Option := %d;\n", CURLOPT_POSTFIELDS);
    printf("   CURLOPT_POSTFIELDSIZE  : constant Curl_Option := %d;\n", CURLOPT_POSTFIELDSIZE);
    printf("\n");
    printf("   CURLINFO_RESPONSE_CODE : constant CURL_Info := %d;\n", CURLINFO_RESPONSE_CODE);
    printf("\n");
#endif
    printf("end Util.Http.Clients.Curl.Constants;\n");

  } else if (argc > 1 && strcmp(argv[1], "types") == 0) {
    struct timespec tv;
#if !defined(_WIN32) && !defined(_MIPS_ARCH)
    struct stat st;
#else
    struct stat64 st;
#endif
      
    printf("with Interfaces.C;\n");
    printf("package Util.Systems.Types is\n");
    printf("\n");
    gen_type("dev_t", UNSIGNED, sizeof(st.st_dev));
    gen_type("ino_t", UNSIGNED, sizeof(st.st_ino));
    gen_type("off_t", SIGNED, sizeof(st.st_size));
#ifndef _WIN32
    gen_type("blksize_t", SIGNED, sizeof(blksize_t));
    gen_type("blkcnt_t", SIGNED, sizeof(blkcnt_t));
    gen_type("uid_t", UNSIGNED, sizeof(uid_t));
    gen_type("gid_t", UNSIGNED, sizeof(gid_t));
    gen_type("nlink_t", UNSIGNED, sizeof(nlink_t));
#else
    gen_type("uid_t", UNSIGNED, sizeof(st.st_uid));
    gen_type("gid_t", UNSIGNED, sizeof(st.st_gid));
    gen_type("nlink_t", UNSIGNED, sizeof(st.st_nlink));
#endif
    gen_type("mode_t", UNSIGNED, sizeof(mode_t));
    printf("\n");
    printf("   S_IFMT   : constant mode_t := 8#%08o#;\n", S_IFMT);
    printf("   S_IFDIR  : constant mode_t := 8#%08o#;\n", S_IFDIR);
    printf("   S_IFCHR  : constant mode_t := 8#%08o#;\n", S_IFCHR);
    printf("   S_IFBLK  : constant mode_t := 8#%08o#;\n", S_IFBLK);
    printf("   S_IFREG  : constant mode_t := 8#%08o#;\n", S_IFREG);
    printf("   S_IFIFO  : constant mode_t := 8#%08o#;\n", S_IFIFO);
    printf("   S_IFLNK  : constant mode_t := 8#%08o#;\n", S_IFLNK);
    printf("   S_IFSOCK : constant mode_t := 8#%08o#;\n", S_IFSOCK);
    printf("   S_ISUID  : constant mode_t := 8#%08o#;\n", S_ISUID);
    printf("   S_ISGID  : constant mode_t := 8#%08o#;\n", S_ISGID);
    printf("   S_IREAD  : constant mode_t := 8#%08o#;\n", S_IREAD);
    printf("   S_IWRITE : constant mode_t := 8#%08o#;\n", S_IWRITE);
    printf("   S_IEXEC  : constant mode_t := 8#%08o#;\n", S_IEXEC);
    printf("\n");
#ifndef _WIN32
    printf("   subtype Time_Type is %s;\n", get_type(UNSIGNED, sizeof(tv.tv_sec)));
    printf("\n");
    printf("   type Timespec is record\n");
    printf("      tv_sec  : Time_Type;\n");
    printf("      tv_nsec : %s;\n", get_type(SIGNED, sizeof(tv.tv_nsec)));
    printf("   end record;\n");
#else
    printf("   subtype Time_Type is %s;\n", get_type(UNSIGNED, sizeof(st.st_mtime)));
    printf("\n");
    printf("   type Timespec is record\n");
    printf("      tv_sec  : Time_Type;\n");
    printf("   end record;\n");
#endif
    printf("   pragma Convention (C_Pass_By_Copy, Timespec);\n\n");

    printf("   type Seek_Mode is (SEEK_SET, SEEK_CUR, SEEK_END);\n");
    printf("   for Seek_Mode use (SEEK_SET => %d, SEEK_CUR => %d, SEEK_END => %d);\n",
           SEEK_SET, SEEK_CUR, SEEK_END);
    printf("\n");
    gen_stat();
    printf("\nend Util.Systems.Types;\n");

  } else {
    printf("with Interfaces.C;\n");
    printf("package Util.Systems.Constants is\n");
    printf("\n");
    printf("   pragma Pure;\n");

    printf("\n   --  Flags used when opening a file with open/creat.\n");
    printf("   O_RDONLY                      : constant Interfaces.C.int := 8#%06o#;\n", O_RDONLY);
    printf("   O_WRONLY                      : constant Interfaces.C.int := 8#%06o#;\n", O_WRONLY);
    printf("   O_RDWR                        : constant Interfaces.C.int := 8#%06o#;\n", O_RDWR);
    printf("   O_CREAT                       : constant Interfaces.C.int := 8#%06o#;\n", O_CREAT);
    printf("   O_EXCL                        : constant Interfaces.C.int := 8#%06o#;\n", O_EXCL);
    printf("   O_TRUNC                       : constant Interfaces.C.int := 8#%06o#;\n", O_TRUNC);
    printf("   O_APPEND                      : constant Interfaces.C.int := 8#%06o#;\n", O_APPEND);
#ifdef O_NONBLOCK
    printf("   O_NONBLOCK                    : constant Interfaces.C.int := 8#%06o#;\n", O_NONBLOCK);
#endif

    printf("\n");

#ifdef F_SETFL
    printf("   --  Flags used by fcntl\n");
    printf("   F_SETFL                       : constant Interfaces.C.int := %d;\n", F_SETFL);
    printf("   F_GETFL                       : constant Interfaces.C.int := %d;\n", F_GETFL);
    printf("   FD_CLOEXEC                    : constant Interfaces.C.int := %d;\n", FD_CLOEXEC);
#endif

#ifdef HAVE_DLFCN_H
    printf("\n");
    printf("   --  Flags used by dlopen\n");
    printf("   RTLD_LAZY                     : constant Interfaces.C.int := 8#%06o#;\n", RTLD_LAZY);
    printf("   RTLD_NOW                      : constant Interfaces.C.int := 8#%06o#;\n", RTLD_NOW);
    printf("   RTLD_NOLOAD                   : constant Interfaces.C.int := 8#%06o#;\n", RTLD_NOLOAD);
    printf("   RTLD_DEEPBIND                 : constant Interfaces.C.int := 8#%06o#;\n", RTLD_DEEPBIND);
    printf("   RTLD_GLOBAL                   : constant Interfaces.C.int := 8#%06o#;\n", RTLD_GLOBAL);
    printf("   RTLD_LOCAL                    : constant Interfaces.C.int := 8#%06o#;\n", RTLD_LOCAL);
    printf("   RTLD_NODELETE                 : constant Interfaces.C.int := 8#%06o#;\n", RTLD_NODELETE);
#endif

    printf("\n");
#ifdef HAVE_DLOPEN
printf("   DLL_OPTIONS : constant String := \"-ldl\";\n");
#else
    printf("   DLL_OPTIONS : constant String := \"\";\n");
#endif
    printf("\n");

    printf("end Util.Systems.Constants;\n");
  }
  
  return 0;
}
