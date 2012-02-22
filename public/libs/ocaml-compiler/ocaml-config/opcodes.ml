(**************************************************************************)
(*                                                                        *)
(*    TypeRex OCaml Studio                                                *)
(*      Thomas Gazagnaire, Fabrice Le Fessant                             *)
(*                                                                        *)
(*    OCaml                                                               *)
(*      Xavier Leroy, projet Cristal, INRIA Rocquencourt                  *)
(*                                                                        *)
(*  Copyright 2011-2012 OCamlPro                                          *)
(*  Copyright 1996-2011 INRIA.                                            *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

let opACC0 = 0
let opACC1 = 1
let opACC2 = 2
let opACC3 = 3
let opACC4 = 4
let opACC5 = 5
let opACC6 = 6
let opACC7 = 7
let opACC = 8
let opPUSH = 9
let opPUSHACC0 = 10
let opPUSHACC1 = 11
let opPUSHACC2 = 12
let opPUSHACC3 = 13
let opPUSHACC4 = 14
let opPUSHACC5 = 15
let opPUSHACC6 = 16
let opPUSHACC7 = 17
let opPUSHACC = 18
let opPOP = 19
let opASSIGN = 20
let opENVACC1 = 21
let opENVACC2 = 22
let opENVACC3 = 23
let opENVACC4 = 24
let opENVACC = 25
let opPUSHENVACC1 = 26
let opPUSHENVACC2 = 27
let opPUSHENVACC3 = 28
let opPUSHENVACC4 = 29
let opPUSHENVACC = 30
let opPUSH_RETADDR = 31
let opAPPLY = 32
let opAPPLY1 = 33
let opAPPLY2 = 34
let opAPPLY3 = 35
let opAPPTERM = 36
let opAPPTERM1 = 37
let opAPPTERM2 = 38
let opAPPTERM3 = 39
let opRETURN = 40
let opRESTART = 41
let opGRAB = 42
let opCLOSURE = 43
let opCLOSUREREC = 44
let opOFFSETCLOSUREM2 = 45
let opOFFSETCLOSURE0 = 46
let opOFFSETCLOSURE2 = 47
let opOFFSETCLOSURE = 48
let opPUSHOFFSETCLOSUREM2 = 49
let opPUSHOFFSETCLOSURE0 = 50
let opPUSHOFFSETCLOSURE2 = 51
let opPUSHOFFSETCLOSURE = 52
let opGETGLOBAL = 53
let opPUSHGETGLOBAL = 54
let opGETGLOBALFIELD = 55
let opPUSHGETGLOBALFIELD = 56
let opSETGLOBAL = 57
let opATOM0 = 58
let opATOM = 59
let opPUSHATOM0 = 60
let opPUSHATOM = 61
let opMAKEBLOCK = 62
let opMAKEBLOCK1 = 63
let opMAKEBLOCK2 = 64
let opMAKEBLOCK3 = 65
let opMAKEFLOATBLOCK = 66
let opGETFIELD0 = 67
let opGETFIELD1 = 68
let opGETFIELD2 = 69
let opGETFIELD3 = 70
let opGETFIELD = 71
let opGETFLOATFIELD = 72
let opSETFIELD0 = 73
let opSETFIELD1 = 74
let opSETFIELD2 = 75
let opSETFIELD3 = 76
let opSETFIELD = 77
let opSETFLOATFIELD = 78
let opVECTLENGTH = 79
let opGETVECTITEM = 80
let opSETVECTITEM = 81
let opGETSTRINGCHAR = 82
let opSETSTRINGCHAR = 83
let opBRANCH = 84
let opBRANCHIF = 85
let opBRANCHIFNOT = 86
let opSWITCH = 87
let opBOOLNOT = 88
let opPUSHTRAP = 89
let opPOPTRAP = 90
let opRAISE = 91
let opCHECK_SIGNALS = 92
let opC_CALL1 = 93
let opC_CALL2 = 94
let opC_CALL3 = 95
let opC_CALL4 = 96
let opC_CALL5 = 97
let opC_CALLN = 98
let opCONST0 = 99
let opCONST1 = 100
let opCONST2 = 101
let opCONST3 = 102
let opCONSTINT = 103
let opPUSHCONST0 = 104
let opPUSHCONST1 = 105
let opPUSHCONST2 = 106
let opPUSHCONST3 = 107
let opPUSHCONSTINT = 108
let opNEGINT = 109
let opADDINT = 110
let opSUBINT = 111
let opMULINT = 112
let opDIVINT = 113
let opMODINT = 114
let opANDINT = 115
let opORINT = 116
let opXORINT = 117
let opLSLINT = 118
let opLSRINT = 119
let opASRINT = 120
let opEQ = 121
let opNEQ = 122
let opLTINT = 123
let opLEINT = 124
let opGTINT = 125
let opGEINT = 126
let opOFFSETINT = 127
let opOFFSETREF = 128
let opISINT = 129
let opGETMETHOD = 130
let opBEQ = 131
let opBNEQ = 132
let opBLTINT = 133
let opBLEINT = 134
let opBGTINT = 135
let opBGEINT = 136
let opULTINT = 137
let opUGEINT = 138
let opBULTINT = 139
let opBUGEINT = 140
let opGETPUBMET = 141
let opGETDYNMET = 142
let opSTOP = 143
let opEVENT = 144
let opBREAK = 145
