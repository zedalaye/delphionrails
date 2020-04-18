(******************************************************************************
* Copyright (C) 1994-2008 Lua.org, PUC-Rio.  All rights reserved.
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)

{$IFDEF FPC}
  {$MODE OBJFPC}{$M+}
  {$DEFINE HAVEINLINE}
{$ENDIF}

unit dorLua;

interface

uses
  AnsiStrings, Classes, superobject;

type
  size_t = Cardinal;
  Psize_t = ^size_t;
{$IFNDEF FPC}
  PtrInt = Int64;
  PtrUInt = UInt64;
{$ENDIF}

const
{$IFDEF UNIX}
  LUA_LIB = 'liblua5.3.5.so';
{$ELSE}
  LUA_LIB = 'lua5.3.5.dll';
{$ENDIF}

const
  LUA_IDSIZE = 60;
  LUAL_BUFFERSIZE = 512;


(*
** $Id: lua.h,v 1.218.1.5 2008/08/06 13:30:12 roberto Exp $
** Lua - An Extensible Extension Language
** Lua.org, PUC-Rio, Brazil (http://www.lua.org)
** See Copyright Notice at the end of this file
*)


// #include "luaconf.h"

const
  LUA_VERSION = 'Lua 5.3';
  LUA_RELEASE = 'Lua 5.3.5';
  LUA_VERSION_NUM = 503;
  LUA_COPYRIGHT = 'Copyright (C) 1994-2018 Lua.org, PUC-Rio';
  LUA_AUTHORS  = 'R. Ierusalimschy, L. H. de Figueiredo, W. Celes';


(* mark for precompiled code (`<esc>Lua') *)
  LUA_SIGNATURE = #$1b'Lua';

(* option for multiple returns in `lua_pcall' and `lua_call' *)
  LUA_MULTRET = -1;


(*
** pseudo-indices
*)
  LUAI_MAXSTACK = 1000000;
  LUA_REGISTRYINDEX = -LUAI_MAXSTACK - 1000;
  function lua_upvalueindex(i: Integer): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}


(* thread status; 0 is OK *)
const
  LUA_OK        = 0;
  LUA_YIELD_    = 1;
  LUA_ERRRUN    = 2;
  LUA_ERRSYNTAX = 3;
  LUA_ERRMEM    = 4;
  LUA_ERRGCMM   = 5;
  LUA_ERRERR    = 6;


type
  Plua_State = ^lua_State;
  lua_State = record
  end;

(*
** basic types
*)
const
  LUA_TNONE  = -1;

  LUA_TNIL           = 0;
  LUA_TBOOLEAN       = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER        = 3;
  LUA_TSTRING        = 4;
  LUA_TTABLE         = 5;
  LUA_TFUNCTION      = 6;
  LUA_TUSERDATA      = 7;
  LUA_TTHREAD        = 8;

  LUA_NUMTAGS        = 9;

(* minimum Lua stack available to a C function *)
  LUA_MINSTACK = 20;

(* predefined values in the registry *)
  LUA_RIDX_MAINTHREAD = 1;
  LUA_RIDX_GLOBALS    = 2;
  LUA_RIDX_LAST       = LUA_RIDX_GLOBALS;

(*
** generic extra include file
*)
//#if defined(LUA_USER_H)
//#include LUA_USER_H
//#endif


type
(* type of numbers in Lua *)
  lua_Number = Double; // LUA_NUMBER


(* type for integer functions *)
  lua_Integer = PtrInt; // LUA_INTEGER

(* unsigned integer type *)
  lua_Unsigned = PtrUint; // LUA_UNSIGNED

(* type for continuation-function contexts *)
  lua_KContext = PtrInt; // LUA_KCONTEXT

(*
** Type for C functions registered with Lua
*)
 lua_CFunction = function(L: Plua_State): Integer; cdecl;

(*
** Type for continuation functions
*)
 lua_KFunction = function(P: Plua_State; status: Integer; ctx: lua_KContext): Integer; cdecl;

(*
** functions that read/write blocks when loading/dumping Lua chunks
*)
  lua_Reader = function(L: Plua_State; ud: Pointer; sz: Psize_t): PAnsiChar; cdecl;
  lua_Writer = function(L: Plua_State; const p: Pointer; sz: Psize_t; ud: Pointer): Integer; cdecl;

(*
** prototype for memory-allocation functions
*)
  lua_Alloc = function(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;

(*
** state manipulation
*)
function lua_newstate(f: lua_Alloc; ud: Pointer): Plua_State; cdecl; external LUA_LIB name 'lua_newstate';
procedure lua_close(L: Plua_State); cdecl; external LUA_LIB name 'lua_close';
function lua_newthread(L: Plua_State): Plua_State; cdecl; external LUA_LIB name 'lua_newthread';

function lua_atpanic(L: Plua_State; panicf: lua_CFunction): lua_CFunction; cdecl; external LUA_LIB name 'lua_atpanic';

function _lua_version(L: Plua_State): lua_Number; cdecl; external LUA_LIB name 'lua_version';

(*
** basic stack manipulation
*)
function lua_absindex(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_absindex';
function lua_gettop(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_gettop';
procedure lua_settop(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_settop';
procedure lua_pushvalue(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_pushvalue';
procedure lua_rotate(L: Plua_State; idx, n: Integer); cdecl; external LUA_LIB name 'lua_rotate';
procedure lua_copy(L: Plua_State; fromidx, toidx: Integer); cdecl; external LUA_LIB name 'lua_copy';
function lua_checkstack(L: Plua_State; n: Integer): Integer; cdecl; external LUA_LIB name 'lua_checkstack';

procedure lua_xmove(from: Plua_State; to_: Plua_State; n: Integer); cdecl; external LUA_LIB name 'lua_xmove';


(*
** access functions (stack -> C)
*)

function lua_isnumber(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_isnumber';
function lua_isstring(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_isstring';
function lua_iscfunction(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_iscfunction';
function lua_isinteger(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_isinteger';
function lua_isuserdata(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_isuserdata';
function lua_type(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_type';
function lua_typename(L: Plua_State; tp: Integer): PAnsiChar; cdecl; external LUA_LIB name 'lua_typename';

function lua_tonumberx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Number; cdecl; external LUA_LIB name 'lua_tonumberx';
function lua_tointegerx(L: Plua_State; idx: Integer; isnum: PInteger): lua_Integer; cdecl; external LUA_LIB name 'lua_tointegerx';
function lua_toboolean(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_toboolean';
function lua_tolstring(L: Plua_State; idx: Integer; len: Psize_t): PAnsiChar; cdecl; external LUA_LIB name 'lua_tolstring';
function lua_rawlen(L: Plua_State; idx: Integer): size_t; cdecl; external LUA_LIB name 'lua_objlen';
function lua_tocfunction(L: Plua_State; idx: Integer): lua_CFunction; cdecl; external LUA_LIB name 'lua_tocfunction';
function lua_touserdata(L: Plua_State; idx: Integer): Pointer; cdecl; external LUA_LIB name 'lua_touserdata';
function lua_tothread(L: Plua_State; idx: Integer): Plua_State; cdecl; external LUA_LIB name 'lua_tothread';
function lua_topointer(L: Plua_State; idx: Integer): Pointer; cdecl; external LUA_LIB name 'lua_topointer';


(*
** Comparison and arithmetic functions
*)

const
  LUA_OPADD	 =  0; (* ORDER TM, ORDER OP *)
  LUA_OPSUB	 =  1;
  LUA_OPMUL	 =  2;
  LUA_OPMOD	 =  3;
  LUA_OPPOW	 =  4;
  LUA_OPDIV	 =  5;
  LUA_OPIDIV =  6;
  LUA_OPBAND =  7;
  LUA_OPBOR	 =  8;
  LUA_OPBXOR =  9;
  LUA_OPSHL	 = 10;
  LUA_OPSHR	 = 11;
  LUA_OPUNM	 = 12;
  LUA_OPBNOT = 13;

procedure lua_arith(L: Plua_State; op: Integer); cdecl; external LUA_LIB name 'lua_arith';

const
  LUA_OPEQ = 0;
  LUA_OPLT = 1;
  LUA_OPLE = 2;

function lua_rawequal(L: Plua_State; idx1: Integer; idx2: Integer): Integer; cdecl; external LUA_LIB name 'lua_rawequal';
function lua_compare(L: Plua_State; idx1: Integer; idx2: Integer; op: Integer): Integer; cdecl; external LUA_LIB name 'lua_compare';


(*
** push functions (C -> stack)
*)
procedure lua_pushnil(L: Plua_State); cdecl; external LUA_LIB name 'lua_pushnil';
procedure lua_pushnumber(L: Plua_State; n: lua_Number); cdecl; external LUA_LIB name 'lua_pushnumber';
procedure lua_pushinteger(L: Plua_State; n: lua_Integer); cdecl; external LUA_LIB name 'lua_pushinteger';
function lua_pushlstring(L: Plua_State; const s: PAnsiChar; len: size_t): PAnsiChar; cdecl; external LUA_LIB name 'lua_pushlstring';
function lua_pushstring(L: Plua_State; const s: PAnsiChar): PAnsiChar; cdecl; external LUA_LIB name 'lua_pushstring';
function lua_pushvfstring(L: Plua_State; const fmt: PAnsiChar): PAnsiChar; varargs; cdecl; external LUA_LIB name 'lua_pushvfstring';
function lua_pushfstring(L: Plua_State; const fmt: PAnsiChar): PAnsiChar; varargs; cdecl; external LUA_LIB name 'lua_pushfstring';
procedure lua_pushcclosure(L: Plua_State; fn: lua_CFunction; n: Integer); cdecl; external LUA_LIB name 'lua_pushcclosure';
procedure lua_pushboolean(L: Plua_State; b: Integer); cdecl; external LUA_LIB name 'lua_pushboolean';
procedure lua_pushlightuserdata(L: Plua_State; p: Pointer); cdecl; external LUA_LIB name 'lua_pushlightuserdata';
function lua_pushthread(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_pushthread';


(*
** get functions (Lua -> stack)
*)
function lua_getglobal(l: Plua_State; const name: PAnsiChar): Integer; cdecl; external LUA_LIB name 'lua_getglobal';
function lua_gettable(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_gettable';
function lua_getfield(L: Plua_State; idx: Integer; const k: PAnsiChar): Integer; cdecl; external LUA_LIB name 'lua_getfield';
function lua_geti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; cdecl; external LUA_LIB name 'lua_geti';
function lua_rawget(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_rawget';
function lua_rawgeti(L: Plua_State; idx: Integer; n: lua_Integer): Integer; cdecl; external LUA_LIB name 'lua_rawgeti';
function lua_rawgetp(L: Plua_State; idx: Integer; const n: Pointer): Integer; cdecl; external LUA_LIB name 'lua_rawgetp';

procedure lua_createtable(L: Plua_State; narr: Integer; nrec: Integer); cdecl; external LUA_LIB name 'lua_createtable';
function lua_newuserdata(L: Plua_State; sz: size_t): Pointer; cdecl; external LUA_LIB name 'lua_newuserdata';
function lua_getmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external LUA_LIB name 'lua_getmetatable';
function lua_getuservalue(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_getuservalue';


(*
** set functions (stack -> Lua)
*)
procedure lua_setglobal(L: Plua_State; const name: PAnsiChar); cdecl; external LUA_LIB name 'lua_setglobal';
procedure lua_settable(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_settable';
procedure lua_setfield(L: Plua_State; idx: Integer; const k: PAnsiChar); cdecl; external LUA_LIB name 'lua_setfield';
procedure lua_seti(L: Plua_State; idx: Integer; n: lua_Integer); cdecl; external LUA_LIB name 'lua_seti';
procedure lua_rawset(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_rawset';
procedure lua_rawseti(L: Plua_State; idx: Integer; n: Integer); cdecl; external LUA_LIB name 'lua_rawseti';
procedure lua_rawsetp(L: Plua_State; idx: Integer; const p: Pointer); cdecl; external LUA_LIB name 'lua_rawsetp';
function lua_setmetatable(L: Plua_State; objindex: Integer): Integer; cdecl; external LUA_LIB name 'lua_setmetatable';
procedure lua_setuservalue(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_setuservalue';


(*
** `load' and `call' functions (load and run Lua code)
*)
procedure lua_callk(L: Plua_State; nargs: Integer; nresults: Integer; ctx: lua_KContext; k: lua_KFunction); cdecl; external LUA_LIB name 'lua_callk';
procedure lua_call(L: Plua_State; nargs: Integer; nresults: Integer); {$IFDEF HAVEINLINE}inline;{$ENDIF}

function lua_pcallk(L: Plua_State; nargs: Integer; nresults: Integer; errfunc: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl; external LUA_LIB name 'lua_pcallk';
function lua_pcall(L: Plua_State; nargs: Integer; nresults: Integer; errfunc: Integer): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}

function lua_load(L: Plua_State; reader: lua_Reader; dt: Pointer; const chunkname, mode: PAnsiChar): Integer; cdecl; external LUA_LIB name 'lua_load';

function lua_dump(L: Plua_State; writer: lua_Writer; data: Pointer; strip: Integer): Integer; cdecl; external LUA_LIB name 'lua_dump';


(*
** coroutine functions
*)
function lua_yieldk(L: Plua_State; nresults: Integer; ctx: lua_KContext; k: lua_KFunction): Integer; cdecl; external LUA_LIB name 'lua_yieldk';
function lua_yield(L: Plua_State; nresults: Integer): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}

function lua_resume(L, from: Plua_State; narg: Integer): Integer; cdecl; external LUA_LIB name 'lua_resume';
function lua_status(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_status';
function lua_isyieldable(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_isyieldable';


(*
** garbage-collection function and options
*)

const
  LUA_GCSTOP       = 0;
  LUA_GCRESTART    = 1;
  LUA_GCCOLLECT    = 2;
  LUA_GCCOUNT      = 3;
  LUA_GCCOUNTB     = 4;
  LUA_GCSTEP       = 5;
  LUA_GCSETPAUSE   = 6;
  LUA_GCSETSTEPMUL = 7;
  LUA_GCISRUNNING  = 9;

function lua_gc(L: Plua_State; what: Integer; data: Integer): Integer; cdecl; external LUA_LIB name 'lua_gc';

(*
** miscellaneous functions
*)

function lua_error(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_error';

function lua_next(L: Plua_State; idx: Integer): Integer; cdecl; external LUA_LIB name 'lua_next';

procedure lua_concat(L: Plua_State; n: Integer); cdecl; external LUA_LIB name 'lua_concat';
procedure lua_len(L: Plua_State; idx: Integer); cdecl; external LUA_LIB name 'lua_len';

function lua_stringtonumber(L: Plua_State; const s: PAnsiChar): size_t; cdecl; external LUA_LIB name 'lua_stringtonumber';

function lua_getallocf(L: Plua_State; var ud: Pointer): lua_Alloc; cdecl; external LUA_LIB name 'lua_getallocf';
procedure lua_setallocf (L: Plua_State; f: lua_Alloc; ud: Pointer); cdecl; external LUA_LIB name 'lua_setallocf ';



(*
** ===============================================================
** some useful macros
** ===============================================================
*)
const
  LUA_EXTRASPACE = SizeOf(Pointer);

function lua_getextraspace(L: Plua_State): Pointer; {$IFDEF HAVEINLINE}inline;{$ENDIF}

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}

procedure lua_pop(L: Plua_State; n: Integer); {$IFDEF HAVEINLINE}inline;{$ENDIF}

procedure lua_newtable(L: Plua_State); {$IFDEF HAVEINLINE}inline;{$ENDIF}

procedure lua_register(L: Plua_State; n: PAnsiChar; f: lua_CFunction); {$IFDEF HAVEINLINE}inline;{$ENDIF}

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction); {$IFDEF HAVEINLINE}inline;{$ENDIF}

function lua_isfunction(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_istable(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_islightuserdata(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_isnil(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_isboolean(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_isthread(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_isnone(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_isnoneornil(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}

procedure lua_pushliteral(L: Plua_State; const s: PAnsiChar); {$IFDEF HAVEINLINE}inline;{$ENDIF}

procedure lua_pushglobaltable(L: Plua_State); {$IFDEF HAVEINLINE}inline;{$ENDIF}

function lua_tostring(L: Plua_State; i: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline;{$ENDIF}

procedure lua_insert(L: Plua_State; idx: Integer); {$IFDEF HAVEINLINE}inline;{$ENDIF}
procedure lua_remove(L: Plua_State; idx: Integer); {$IFDEF HAVEINLINE}inline;{$ENDIF}
procedure lua_replace(L: Plua_State; idx: Integer); {$IFDEF HAVEINLINE}inline;{$ENDIF}

(*
** {======================================================================
** Debug API
** =======================================================================
*)


(*
** Event codes
*)
const
  LUA_HOOKCALL     = 0;
  LUA_HOOKRET      = 1;
  LUA_HOOKLINE     = 2;
  LUA_HOOKCOUNT    = 3;
  LUA_HOOKTAILCALL = 4;


(*
** Event masks
*)
  LUA_MASKCALL  = (1 shl LUA_HOOKCALL);
  LUA_MASKRET   = (1 shl LUA_HOOKRET);
  LUA_MASKLINE  = (1 shl LUA_HOOKLINE);
  LUA_MASKCOUNT = (1 shl LUA_HOOKCOUNT);

type
  Plua_Debug = ^lua_Debug;
  lua_Debug = record
    event: Integer;
    name: PAnsiChar; (* (n) *)
    namewhat: PAnsiChar; (* (n) `global', `local', `field', `method' *)
    what: PAnsiChar; (* (S) `Lua', `C', `main', `tail' *)
    source: PAnsiChar; (* (S) *)
    currentline: Integer; (* (l) *)
    linedefined: Integer; (* (S) *)
    lastlinedefined: Integer; (* (S) *)
    nups: Byte;  (* (u) number of upvalues *)
    nparams: Byte;  (* (u) number of parameters *)
    isvararg: Byte;
    istailcall: Byte;
    short_src: array[0..LUA_IDSIZE-1] of AnsiChar; (* (S) *)
    (* private part *)
    i_ci: Pointer;  (* active function *)
  end; (* activation record *)


(* Functions to be called by the debuger in specific events *)
  lua_Hook = procedure(L: Plua_State; ar: Plua_Debug); cdecl;

function lua_getstack(L: Plua_State; level: Integer; ar: Plua_Debug): Integer; cdecl; external LUA_LIB name 'lua_getstack';
function lua_getinfo(L: Plua_State; const what: PAnsiChar; ar: Plua_Debug): Integer; cdecl; external LUA_LIB name 'lua_getinfo';
function lua_getlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PAnsiChar; cdecl; external LUA_LIB name 'lua_getlocal';
function lua_setlocal(L: Plua_State; const ar: Plua_Debug; n: Integer): PAnsiChar; cdecl; external LUA_LIB name 'lua_setlocal';
function lua_getupvalue(L: Plua_State; funcindex: Integer; n: Integer): PAnsiChar; cdecl; external LUA_LIB name 'lua_getupvalue';
function lua_setupvalue(L: Plua_State; funcindex: Integer; n: Integer): PAnsiChar; cdecl; external LUA_LIB name 'lua_setupvalue';

function lua_upvalueid(L: Plua_State; fidx: Integer; n: Integer): Pointer; cdecl; external LUA_LIB name 'lua_upvalueid';
procedure lua_upvaluejoin(L: Plua_State; fidx1: Integer; n1: Integer; fidx2: Integer; n2: Integer); cdecl; external LUA_LIB name 'lua_upvaluejoin';

procedure lua_sethook(L: Plua_State; func: lua_Hook; mask: Integer; count: Integer); cdecl; external LUA_LIB name 'lua_sethook';
function lua_gethook(L: Plua_State): lua_Hook; cdecl; external LUA_LIB name 'lua_gethook';
function lua_gethookmask(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_gethookmask';
function lua_gethookcount(L: Plua_State): Integer; cdecl; external LUA_LIB name 'lua_gethookcount';


(* }====================================================================== *)


(*
** $Id: lauxlib.h,v 1.88.1.1 2007/12/27 13:02:25 roberto Exp $
** Auxiliary functions for building Lua libraries
** See Copyright Notice in lua.h
*)


//#if defined(LUA_COMPAT_GETN)
//LUALIB_API int (luaL_getn) (lua_State *L, int t);
//LUALIB_API void (luaL_setn) (lua_State *L, int t, int n);
//#else
function luaL_getn(L: Plua_State; idx: Integer): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}
//#define luaL_setn(L,i,j)        ((void)0)  (* no op! *)
//#endif

//#if defined(LUA_COMPAT_OPENLIB)
//#define luaI_openlib	luaL_openlib
//#endif


const
(* extra error code for `luaL_loadfilex' *)
  LUA_ERRFILE = LUA_ERRERR + 1;
(* key, in the registry, for table of loaded modules *)
  LUA_LOADED_TABLE = '_LOADED';
(* key, in the registry, for table of preloaded loaders *)
  LUA_PRELOAD_TABLE	= '_PRELOAD';

type
  PluaL_Reg = ^luaL_Reg;
  luaL_Reg = record
    name: PAnsiChar;
    func: lua_CFunction;
  end;

const
  LUAL_NUMSIZES	= (SizeOf(lua_Integer)*16 + SizeOf(lua_Number));

procedure luaL_checkversion_(L: Plua_State; ver: lua_Number; sz: size_t); cdecl; external LUA_LIB name 'luaL_checkversion_';
procedure luaL_checkversion(L: Plua_State);

function luaL_getmetafield(L: Plua_State; obj: Integer; const e: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_getmetafield';
function luaL_callmeta(L: Plua_State; obj: Integer; const e: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_callmeta';
function luaL_tolstring(L: Plua_State; idx: Integer; var len: size_t): PAnsiChar; cdecl; external LUA_LIB name 'luaL_tolstring';
function luaL_argerror(L: Plua_State; numarg: Integer; const extramsg: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_argerror';

function luaL_checklstring(L: Plua_State; numArg: Integer; len: Psize_t): PAnsiChar; cdecl; external LUA_LIB name 'luaL_checklstring';
function luaL_optlstring(L: Plua_State; numArg: Integer; const def: PAnsiChar; len: Psize_t): PAnsiChar; cdecl; external LUA_LIB name 'luaL_optlstring';

function luaL_checknumber(L: Plua_State; numArg: Integer): lua_Number; cdecl; external LUA_LIB name 'luaL_checknumber';
function luaL_optnumber(L: Plua_State; nArg: Integer; def: lua_Number): lua_Number; cdecl; external LUA_LIB name 'luaL_optnumber';

function luaL_checkinteger(L: Plua_State; arg: Integer): lua_Integer; cdecl; external LUA_LIB name 'luaL_checkinteger';
function luaL_optinteger(L: Plua_State; arg: Integer; def: lua_Integer): lua_Integer; cdecl; external LUA_LIB name 'luaL_optinteger';

procedure luaL_checkstack(L: Plua_State; sz: Integer; const msg: PAnsiChar); cdecl; external LUA_LIB name 'luaL_checkstack';
procedure luaL_checktype(L: Plua_State; narg: Integer; t: Integer); cdecl; external LUA_LIB name 'luaL_checktype';
procedure luaL_checkany(L: Plua_State; narg: Integer); cdecl; external LUA_LIB name 'luaL_checkany';

function luaL_newmetatable(L: Plua_State; const tname: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_newmetatable';
procedure luaL_setmetatable(L: Plua_State; const tname: PAnsiChar); cdecl; external LUA_LIB name 'luaL_setmetatable';

function luaL_testudata(L: Plua_State; ud: Integer; const tname: PAnsiChar): Pointer; cdecl; external LUA_LIB name 'luaL_testudata';
function luaL_checkudata(L: Plua_State; ud: Integer; const tname: PAnsiChar): Pointer; cdecl; external LUA_LIB name 'luaL_checkudata';

procedure luaL_where(L: Plua_State; lvl: Integer); cdecl; external LUA_LIB name 'luaL_where';
function luaL_error(L: Plua_State; const fmt: PAnsiChar): Integer; varargs; cdecl; external LUA_LIB name 'luaL_error';

function luaL_checkoption(L: Plua_State; narg: Integer; const def: PAnsiChar; const lst: PPAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_checkoption';

function luaL_fileresult(L: Plua_State; stat: Integer; const fname: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_fileresult';
function luaL_execresult(L: Plua_State; stat: Integer): Integer; cdecl; external LUA_LIB name 'luaL_execresult';

const
  LUA_NOREF = -2;
  LUA_REFNIL = -1;

function luaL_ref(L: Plua_State; t: Integer): Integer; cdecl; external LUA_LIB name 'luaL_ref';
procedure luaL_unref(L: Plua_State; t: Integer; ref: Integer); cdecl; external LUA_LIB name 'luaL_unref';

function luaL_loadfilex(L: Plua_State; const filename, mode: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_loadfilex';
function luaL_loadfile(L: Plua_State; const filename: PAnsiChar): Integer;

function luaL_loadbufferx(L: Plua_State; const buff: PAnsiChar; sz: size_t; const name, mode: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_loadbufferx';
function luaL_loadbuffer(L: Plua_State; const buff: PAnsiChar; sz: size_t; const name: PAnsiChar): Integer;

function luaL_loadstring(L: Plua_State; const s: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_loadstring';

function luaL_newstate: Plua_State; cdecl; external LUA_LIB name 'luaL_newstate:';

function luaL_len(L: Plua_State; idx: Integer): lua_Integer; cdecl; external LUA_LIB name 'luaL_len';

function luaL_gsub(L: Plua_State; const s: PAnsiChar; const p: PAnsiChar; const r: PAnsiChar): PAnsiChar; cdecl; external LUA_LIB name 'luaL_gsub';

procedure luaL_setfuncs(L: Plua_State; const _l: PluaL_Reg; nup: Integer); cdecl; external LUA_LIB name 'luaL_setfuncs';

function luaL_getsubtable(L: Plua_State; idx: Integer; const fname: PAnsiChar): Integer; cdecl; external LUA_LIB name 'luaL_getsubtable';

procedure luaL_traceback(L, L1: Plua_State; const msg: PAnsiChar; level: Integer); cdecl; external LUA_LIB name 'luaL_traceback';

procedure luaL_requiref(L: Plua_State; const modname: PAnsiChar; openf: lua_CFunction; glb: Integer); cdecl; external LUA_LIB name 'luaL_requiref';

(*
** ===============================================================
** some useful macros
** ===============================================================
*)

//#define luaL_newlibtable(L,l)	\
//  lua_createtable(L, 0, sizeof(l)/sizeof((l)[0]) - 1)

//#define luaL_newlib(L,l)  \
//  (luaL_checkversion(L), luaL_newlibtable(L,l), luaL_setfuncs(L,l,0))

function luaL_argcheck(L: Plua_State; cond: Boolean; numarg: Integer; const extramsg: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function luaL_checkstring(L: Plua_State; n: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function luaL_optstring(L: Plua_State; n: Integer; d: PAnsiChar): PAnsiChar; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function luaL_typename(L: Plua_State; i: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function luaL_dofile(L: Plua_State; fn: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function luaL_dostring(L: Plua_State; s: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
procedure luaL_getmetatable(L: Plua_State; n: PAnsiChar); {$IFDEF HAVEINLINE}inline;{$ENDIF}
//#define luaL_opt(L,f,n,d) (lua_isnoneornil(L,(n)) ? (d) : f(L,(n)))

(*
** {======================================================
** Generic Buffer manipulation
** =======================================================
*)

type
  PluaL_Buffer = ^luaL_Buffer;
  luaL_Buffer = record
    b: PAnsiChar; (* buffer address *)
    size: size_t; (* buffer size *)
    n: size_t;    (* number of characters in buffer *)
    L: Plua_State;
    initb: array[0..LUAL_BUFFERSIZE-1] of AnsiChar;
  end;

//#define luaL_addchar(B,c) \
//  ((void)((B)->p < ((B)->buffer+LUAL_BUFFERSIZE) || luaL_prepbuffer(B)), \
//   (*(B)->p++ = (char)(c)))

//#define luaL_addsize(B,n) ((B)->p += (n))

procedure luaL_buffinit(L: Plua_State; B: PluaL_Buffer); cdecl; external LUA_LIB name 'luaL_buffinit';
function luaL_prepbuffsize(B: PluaL_Buffer; sz: size_t): PAnsiChar; cdecl; external LUA_LIB name 'luaL_prepbuffsize';
procedure luaL_addlstring(B: PluaL_Buffer; const s: PAnsiChar; l: size_t); cdecl; external LUA_LIB name 'luaL_addlstring';
procedure luaL_addstring(B: PluaL_Buffer; const s: PAnsiChar); cdecl; external LUA_LIB name 'luaL_addstring';
procedure luaL_addvalue(B: PluaL_Buffer); cdecl; external LUA_LIB name 'luaL_addvalue';
procedure luaL_pushresult(B: PluaL_Buffer); cdecl; external LUA_LIB name 'luaL_pushresult';
procedure luaL_pushresultsize(B: PluaL_Buffer; sz: size_t); cdecl; external LUA_LIB name 'luaL_pushresultsize';
function luaL_buffinitsize(L: Plua_State; B: PluaL_Buffer; sz: size_t): PAnsiChar; cdecl; external LUA_LIB name 'luaL_buffinitsize';

function luaL_prepbuffer(B: PluaL_Buffer): PAnsiChar;


(* }====================================================== *)


(*
** {======================================================
** File handles for IO library
** =======================================================
*)

(*
** A file handle is a userdata with metatable 'LUA_FILEHANDLE' and
** initial structure 'luaL_Stream' (it may contain other fields
** after that initial structure).
*)

const
  LUA_FILEHANDLE = 'FILE*';

type
  PluaL_Stream = ^luaL_Stream;
  luaL_Stream = record
    f: Pointer; (* stream (NULL for incompletely created streams) *)
    closef: lua_CFunction; (* to close stream (NULL for closed streams) *)
  end;

(* }====================================================== *)



(* Key to file-handle type *)
const
  LUA_COLIBNAME   = 'coroutine';
  LUA_TABLIBNAME  = 'table';
  LUA_IOLIBNAME   = 'io';
  LUA_OSLIBNAME   = 'os';
  LUA_STRLIBNAME  = 'string';
  LUA_UTF8LIBNAME = 'utf8';
  LUA_BITLIBNAME  = 'bit32';
  LUA_MATHLIBNAME = 'math';
  LUA_DBLIBNAME   = 'debug';
  LUA_LOADLIBNAME = 'package';

function luaopen_base(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_base';
function luaopen_coroutine(L: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_coroutine';
function luaopen_table(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_table';
function luaopen_io(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_io';
function luaopen_os(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_os';
function luaopen_string(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_string';
function luaopen_utf8(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_utf8';
function luaopen_bit32(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_bit32';
function luaopen_math(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_math';
function luaopen_debug(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_debug';
function luaopen_package(l: Plua_State): Integer; cdecl; external LUA_LIB name 'luaopen_package';

(* open all previous libraries *)
procedure luaL_openlibs(L: Plua_State); cdecl; external LUA_LIB name 'luaL_openlibs';


// DoR related utilities

procedure lua_pushsuperobject(L: Plua_State; const obj: ISuperObject);
function lua_tosuperobject(L: Plua_State; idx: Integer): ISuperObject;
function lua_app_alloc(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;
function lua_processsor_loadstream(L: Plua_State; stream: TStream; chunkname: PAnsiChar; mode: PAnsiChar): Integer;
function lua_processsor_loadfile(L: Plua_State; const FileName: string; chunkname: PAnsiChar; mode: PAnsiChar): Integer;
function lua_processsor_dofile(L: Plua_State; const FileName: string; chunkname: PAnsiChar; mode: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
function lua_processsor_dostream(L: Plua_State; stream: TStream; chunkname: PAnsiChar; mode: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}

implementation

uses
  SysUtils, dorDB;

procedure lua_call(L: Plua_State; nargs: Integer; nresults: Integer);
begin
  lua_callk(L, nargs, nresults, 0, nil);
end;

function lua_pcall(L: Plua_State; nargs: Integer; nresults: Integer; errfunc: Integer): Integer;
begin
  Result := lua_pcallk(L, nargs, nresults, errfunc, 0, nil);
end;

function lua_yield(L: Plua_State; nresults: Integer): Integer;
begin
  Result := lua_yieldk(L, nresults, 0, nil);
end;

function lua_getextraspace(L: Plua_State): Pointer;
begin
  Result := Pointer(PByte(L) - LUA_EXTRASPACE);
end;

function lua_tonumber(L: Plua_State; idx: Integer): lua_Number;
begin
  Result := lua_tonumberx(L, idx, nil);
end;

function lua_tointeger(L: Plua_State; idx: Integer): lua_Integer;
begin
  Result := lua_tointegerx(L, idx, nil);
end;

procedure lua_pop(L: Plua_State; n: Integer);  {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_settop(L, -(n)-1)
end;

procedure lua_newtable(L: Plua_State); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_createtable(L, 0, 0)
end;

procedure lua_register(L: Plua_State; n: PAnsiChar; f: lua_CFunction); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_pushcfunction(L, f);
  lua_setglobal(L, n);
end;

procedure lua_pushcfunction(L: Plua_State; f: lua_CFunction); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_pushcclosure(L, f, 0)
end;

function lua_isfunction(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_type(L, n) = LUA_TFUNCTION;
end;

function lua_istable(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_type(L, n) = LUA_TTABLE;
end;

function lua_islightuserdata(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_type(L, n) = LUA_TLIGHTUSERDATA;
end;

function lua_isnil(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_type(L, n) = LUA_TNIL;
end;

function lua_isboolean(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_type(L, n) = LUA_TBOOLEAN;
end;

function lua_isthread(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_type(L, n) = LUA_TTHREAD;
end;

function lua_isnone(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_type(L, n) = LUA_TNONE;
end;

function lua_isnoneornil(L: Plua_State; n: Integer): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_type(L, n) <= 0;
end;

procedure lua_pushglobaltable(L: Plua_State); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_rawgeti(L, LUA_REGISTRYINDEX, LUA_RIDX_GLOBALS);
end;

procedure lua_insert(L: Plua_State; idx: Integer); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_rotate(L, idx, 1);
end;

procedure lua_remove(L: Plua_State; idx: Integer); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_rotate(L, idx, -1);
  lua_pop(L, 1)
end;

procedure lua_replace(L: Plua_State; idx: Integer); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_copy(L, -1, idx);
  lua_pop(L, 1)
end;

function lua_tostring(L: Plua_State; i: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_tolstring(L, i, nil);
end;

procedure lua_pushliteral(L: Plua_State; const s: PAnsiChar); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_pushlstring(L, s, AnsiStrings.StrLen(s))
end;

procedure lua_getregistry(L: Plua_State); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_pushvalue(L, LUA_REGISTRYINDEX);
end;

function lua_getgccount(L: Plua_State): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_gc(L, LUA_GCCOUNT, 0);
end;

function lua_upvalueindex(i: Integer): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := LUA_REGISTRYINDEX - i;
end;


procedure luaL_checkversion(L: Plua_State);
begin
  luaL_checkversion_(L, LUA_VERSION_NUM, LUAL_NUMSIZES);
end;

function luaL_loadfile(L: Plua_State; const filename: PAnsiChar): Integer;
begin
  Result := luaL_loadfilex(L, filename, nil);
end;

function luaL_loadbuffer(L: Plua_State; const buff: PAnsiChar; sz: size_t; const name: PAnsiChar): Integer;
begin
  Result := luaL_loadbufferx(L, buff, sz, name, nil);
end;

function luaL_argcheck(L: Plua_State; cond: Boolean; numarg: Integer; const extramsg: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := cond or (luaL_argerror(L, numarg, extramsg) <> 0)
end;

function luaL_checkstring(L: Plua_State; n: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := luaL_checklstring(L, n, nil);
end;

function luaL_optstring(L: Plua_State; n: Integer; d: PAnsiChar): PAnsiChar; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := luaL_optlstring(L, n, d, nil)
end;

function luaL_getn(L: Plua_State; idx: Integer): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_rawlen(L, idx);
end;

function luaL_checkint(L: Plua_State; n: Integer): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := luaL_checkinteger(L, n);
end;

function luaL_optint(L: Plua_State; n: Integer; d: lua_Integer): Integer; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := luaL_optinteger(L, n, d);
end;

function luaL_checklong(L: Plua_State; n: Integer): LongInt; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := luaL_checkinteger(L, n);
end;

function luaL_optlong(L: Plua_State; n: Integer; d: lua_Integer): LongInt; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := luaL_optinteger(L, n, d);
end;

function luaL_typename(L: Plua_State; i: Integer): PAnsiChar; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := lua_typename(L, lua_type(L,i));
end;

function luaL_dofile(L: Plua_State; fn: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := (luaL_loadfile(L, fn) <> 0) or (lua_pcall(L, 0, LUA_MULTRET, 0) <> 0);
end;

function luaL_dostring(L: Plua_State; s: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := (luaL_loadstring(L, s) <> 0) or (lua_pcall(L, 0, LUA_MULTRET, 0) <> 0);
end;

procedure luaL_getmetatable(L: Plua_State; n: PAnsiChar); {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  lua_getfield(L, LUA_REGISTRYINDEX, n);
end;

function luaL_prepbuffer(B: PluaL_Buffer): PAnsiChar;
begin
  Result := luaL_prepbuffsize(B, LUAL_BUFFERSIZE);
end;

(* DOR Specific *)

function lua_tosuperobject(L: Plua_State; idx: Integer): ISuperObject;
var
  i: Integer;
  obj: ISuperObject;
  vi: lua_Integer;
  vd: Double;
begin
  Result := nil;
  case lua_type(L, idx) of
    LUA_TNIL:
      Result := nil;
    LUA_TBOOLEAN:
      Result := TSuperObject.Create(lua_toboolean(L, idx) <> 0);
    LUA_TNUMBER:
      begin
        vi := lua_tointeger(L, idx);
        vd := lua_tonumber(L, idx);
        if vi = vd then
          Result := TSuperObject.Create(vi)
        else
          Result := TSuperObject.Create(vd);
      end;
    LUA_TSTRING:
      Result := TSuperObject.Create({$IFDEF UNICODE}UTF8ToUnicodeString{$ELSE}UTF8Decode{$ENDIF}(lua_tostring(L, idx)));
    LUA_TTABLE:
      begin
        lua_pushvalue(L, idx);
        lua_pushnil(L);
        while (lua_next(L, -2) <> 0) do
        begin
          case lua_type(L, -2) of
            LUA_TNUMBER:
            begin
              if Result = nil then
              begin
                Result := TSuperObject.Create(stArray);
                Result.AsArray[lua_tointeger(L, -2) - 1] := lua_tosuperobject(L, -1);
              end
              else if ObjectIsType(Result, stArray) then
                Result.AsArray[lua_tointeger(L, -2) - 1] := lua_tosuperobject(L, -1)
              else
                Result.AsObject[IntToStr(lua_tointeger(L, -2) - 1)] := lua_tosuperobject(L, -1);
            end;
            LUA_TSTRING:
            begin
              if Result = nil then
              begin
                Result := TSuperObject.Create(stObject);
                Result.AsObject[{$IFDEF UNICODE}UTF8ToUnicodeString{$ELSE}UTF8Decode{$ENDIF}(lua_tostring(L, -2))] := lua_tosuperobject(L, -1);
              end
              else if ObjectIsType(Result, stObject) then
                Result.AsObject[{$IFDEF UNICODE}UTF8ToUnicodeString{$ELSE}UTF8Decode{$ENDIF}(lua_tostring(L, -2))] := lua_tosuperobject(L, -1)
              else
              begin
                obj := TSuperObject.Create(stObject);
                for i := 0 to Result.AsArray.Length - 1 do
                  obj.AsObject[inttostr(i)] := Result.AsArray[i];
                Result := obj;
                Result.AsObject[{$IFDEF UNICODE}UTF8ToUnicodeString{$ELSE}UTF8Decode{$ENDIF}(lua_tostring(L, -2))] := lua_tosuperobject(L, -1)
              end;
            end;
          end;

          if Result = nil then
            Result := TSuperObject.Create(stArray);

          lua_pop(L, 1);
        end;
        lua_pop(L, 1);
      end;
  end;
end;

procedure lua_pushsuperobject(L: Plua_State; const obj: ISuperObject);
var
  i, len: Integer;
  ar: TSuperArray;
  ite: TSuperObjectIter;
  dt: IDBDateTime;
begin
  if (obj <> nil) and obj.Processing then
    lua_pushnil(L) else
  begin
    if obj <> nil then
      obj.Processing := true;
    try
      case ObjectGetType(obj) of
        stNull:
          lua_pushnil(L);
        stBoolean:
          lua_pushboolean(L, obj.AsInteger);
        stDouble, stCurrency:
          lua_pushnumber(L, obj.AsDouble);
        stInt:
          begin
            if obj.QueryInterface(IDBDateTime, dt) <> 0 then
              lua_pushinteger(L, obj.AsInteger)
            else
            begin
              lua_pushinteger(L, obj.AsInteger div 1000);
              dt := nil;
            end;
          end;
        stString:
          lua_pushstring(L, PAnsiChar(UTF8Encode(obj.AsString)));
        stObject:
          begin
            len := obj.AsObject.count;
            lua_createtable(L, 0, len);
            if ObjectFindFirst(obj, ite) then
            repeat
              if not ObjectIsType(ite.val, stMethod) then
              begin
                lua_pushstring(L, PAnsiChar(UTF8Encode(ite.key)));
                lua_pushsuperobject(L, ite.val);
                lua_settable(L, -3);
              end;
            until not ObjectFindNext(ite);
            ObjectFindClose(ite);
          end;
        stArray:
          begin
            ar := obj.AsArray;
            len := ar.Length;
            lua_createtable(L, len, 0);
            for i := 0 to len-1 do
            begin
              lua_pushnumber(L, i+1);
              lua_pushsuperobject(L, ar[i]);
              lua_settable(L, -3);
            end;
          end;
        stMethod:
          lua_pushnil(L);
      end;
    finally
      if obj <> nil then
        obj.Processing := false;
    end;
  end;
end;

function lua_app_alloc(ud, ptr: Pointer; osize, nsize: size_t): Pointer; cdecl;
begin
  if (nsize > 0) then
  begin
    if ptr = nil then
      GetMem(Result, nsize)
    else
    begin
      ReallocMem(ptr, nsize);
      Result := ptr;
    end;
  end
  else
  begin
    if ptr <> nil then
      FreeMem(ptr);
    Result := nil;
  end;
end;

type
  TLuaState = (
    lsStart,
    lsLessThan,
    lsEscape,
    lsEscapeClose,
    lsLua,
    lsLuaBody,
    lsLuaBodyEnd,
    lsLuaEqual,
    lsLuaEqualEnd
  );
  PLuaTextProcessor = ^TLuaTextProcessor;
  TLuaTextProcessor = record
    stream: TStream;
    state: TLuaState;
    outbuffer: array[0..1023] of AnsiChar;
  end;

function lua_stream_reader(L: Plua_State; ud: Pointer; sz: Psize_t): PAnsiChar; cdecl;
var
  inbuffer: array[0..1023] of AnsiChar;
  inlen, outlen: Integer;
  pr: PLuaTextProcessor;
  pin, pout: PAnsiChar;
  c: AnsiChar;
  function Append(const str: PAnsiChar; l: Integer): Boolean;
  begin
    if outlen + l <= sizeof(pr.outbuffer) then
    begin
      move(str^, pout^, l);
      inc(pout, l);
      inc(outlen, l);
      Result := true;
    end else
      Result := False;
  end;
label
  redo, needspace;
begin
  pr := PLuaTextProcessor(ud);
  inlen := pr.stream.Read(inbuffer, sizeof(inbuffer));
  outlen := 0;
  pin := @inbuffer;
  pout := @pr.outbuffer;
  while (inlen > 0) do
  begin
    c := pin^;
redo:
    case pr.state of
      lsStart:
        begin
          case c of
            '<': pr.state := lsLessThan;
          else
            if not Append('print("', 7) then goto needspace;
            pr.state := lsEscape;
            goto redo;
          end;
        end;
      lsLessThan:
        begin
          case c of
            '%': pr.state := lslua;
          else
            if not Append('print("<', 8) then goto needspace;
            pr.state := lsEscape;
            goto redo;
          end;
        end;
      lsEscape:
        begin
          case c of
            '<': pr.state := lsEscapeClose;
          else
            case c of
              #7 : if not Append('\a', 2) then goto needspace;
              #8 : if not Append('\b', 2) then goto needspace;
              #9 : if not Append('\t', 2) then goto needspace;
              #10: if not Append('\n")'#10'print("', 12) then goto needspace;
              #11: if not Append('\v', 2) then goto needspace;
              #13: if not Append('\r', 2) then goto needspace;
              '\': if not Append('\\', 2) then goto needspace;
              '"': if not Append('\"', 2) then goto needspace;
              '''': if not Append('\''', 2) then goto needspace;
            else
              if not Append(pin, 1) then goto needspace;
            end;
          end;
        end;
      lsEscapeClose:
        begin
          case c of
            '%':
              begin
                if not Append('");', 3) then goto needspace;
                pr.state := lsLua;
              end;
          else
            if not Append('<', 1) then goto needspace;
            pr.state := lsEscape;
            goto redo;
          end;
        end;
      lsLua:
        begin
          case c of
            '=':
              begin
                if not Append('print((', 7) then goto needspace;
                pr.state := lsLuaEqual;
              end;
            '%': pr.state := lsLuaBodyEnd;
          else
            if not Append(pin, 1) then goto needspace;
            pr.state := lsLuaBody;
          end;
        end;
      lsLuaBody:
        begin
          case c of
            '%': pr.state := lsLuaBodyEnd;
          else
            if not Append(pin, 1) then goto needspace;
          end;
        end;
      lsLuaBodyEnd:
        begin
          case c of
            '>': pr.state := lsStart;
          else
            if not Append('%', 1) then goto needspace;
            pr.state := lsLuaBody;
            goto redo;
          end;
        end;
      lsLuaEqual:
        begin
          case c of
            '%': pr.state := lsLuaEqualEnd;
          else
            if not Append(pin, 1) then goto needspace;
          end;
        end;
      lsLuaEqualEnd:
        begin
          case c of
            '>':
              begin
                if not append('));', 3) then goto needspace;
                pr.state := lsStart;
              end;
          else
            if not Append('%', 1) then goto needspace;
            pr.state := lsLuaEqual;
            goto redo;
          end;
        end;
    end;
    inc(pin);
    dec(inlen);
  end;
  if outlen = 0 then
  begin
    case pr.state of
      lsLessThan: Append('print("<")', 10);
      lsEscape: Append('")', 2);
      lsEscapeClose: Append('%")', 3);
    end;
    pr.state := lsStart;
  end;
  Result := @pr.outbuffer;
  sz^ := outlen;
  Exit;
needspace:
  pr.stream.Seek(-inlen, soFromCurrent);
  Result := @pr.outbuffer;
  sz^ := outlen;
end;

function lua_processsor_loadstream(L: Plua_State; stream: TStream; chunkname: PAnsiChar; mode: PAnsiChar): Integer;
var
  bom: array[0..2] of Byte;
  processor: TLuaTextProcessor;
begin
  processor.stream := stream;
  processor.state := lsStart;
  // skip utf8 bom
  stream.Seek(0, soFromBeginning);
  if not((stream.Read(bom, 3) = 3) and (bom[0] = $EF) and (bom[1] = $BB) and (bom[2] = $BF)) then
    stream.Seek(0, soFromBeginning);
  Result := lua_load(L, @lua_stream_reader, @processor, chunkname, mode);
end;

function lua_processsor_loadfile(L: Plua_State; const FileName: string; chunkname: PAnsiChar; mode: PAnsiChar): Integer;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := lua_processsor_loadstream(L, stream, chunkname, mode);
  finally
    stream.Free;
  end;
end;

function lua_processsor_dofile(L: Plua_State; const FileName: string; chunkname: PAnsiChar; mode: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := (lua_processsor_loadfile(L, FileName, chunkname, mode) = 0) and (lua_pcall(L, 0, LUA_MULTRET, 0) = 0);
end;

function lua_processsor_dostream(L: Plua_State; stream: TStream; chunkname: PAnsiChar; mode: PAnsiChar): Boolean; {$IFDEF HAVEINLINE}inline;{$ENDIF}
begin
  Result := (lua_processsor_loadstream(L, stream, chunkname, mode) = 0) and (lua_pcall(L, 0, LUA_MULTRET, 0) = 0);
end;

end.
