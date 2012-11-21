module Language.Java.Binary.Opcodes
       ( Opcode (..)
       ) where

import Language.Java.Binary.Bytes

import Data.Binary

data Opcode =
  NOP             |
  ACONST_NULL     |
  ICONST_M1       |
  ICONST_0        |
  ICONST_1        |
  ICONST_2        |
  ICONST_3        |
  ICONST_4        |
  ICONST_5        |
  LCONST_0        |
  LCONST_1        |
  FCONST_0        |
  FCONST_1        |
  FCONST_2        |
  DCONST_0        |
  DCONST_1        |
  BIPUSH          |
  SIPUSH          |
  LDC             |
  LDC_W           |
  LDC2_W          |
  ILOAD           |
  LLOAD           |
  FLOAD           |
  DLOAD           |
  ALOAD           |
  ILOAD_0         |
  ILOAD_1         |
  ILOAD_2         |
  ILOAD_3         |
  LLOAD_0         |
  LLOAD_1         |
  LLOAD_2         |
  LLOAD_3         |
  FLOAD_0         |
  FLOAD_1         |
  FLOAD_2         |
  FLOAD_3         |
  DLOAD_0         |
  DLOAD_1         |
  DLOAD_2         |
  DLOAD_3         |
  ALOAD_0         |
  ALOAD_1         |
  ALOAD_2         |
  ALOAD_3         |
  IALOAD          |
  LALOAD          |
  FALOAD          |
  DALOAD          |
  AALOAD          |
  BALOAD          |
  CALOAD          |
  SALOAD          |
  ISTORE          |
  LSTORE          |
  FSTORE          |
  DSTORE          |
  ASTORE          |
  ISTORE_0        |
  ISTORE_1        |
  ISTORE_2        |
  ISTORE_3        |
  LSTORE_0        |
  LSTORE_1        |
  LSTORE_2        |
  LSTORE_3        |
  FSTORE_0        |
  FSTORE_1        |
  FSTORE_2        |
  FSTORE_3        |
  DSTORE_0        |
  DSTORE_1        |
  DSTORE_2        |
  DSTORE_3        |
  ASTORE_0        |
  ASTORE_1        |
  ASTORE_2        |
  ASTORE_3        |
  IASTORE         |
  LASTORE         |
  FASTORE         |
  DASTORE         |
  AASTORE         |
  BASTORE         |
  CASTORE         |
  SASTORE         |
  POP             |
  POP2            |
  DUP             |
  DUP_X1          |
  DUP_X2          |
  DUP2            |
  DUP2_X1         |
  DUP2_X2         |
  SWAP            |
  IADD            |
  LADD            |
  FADD            |
  DADD            |
  ISUB            |
  LSUB            |
  FSUB            |
  DSUB            |
  IMUL            |
  LMUL            |
  FMUL            |
  DMUL            |
  IDIV            |
  LDIV            |
  FDIV            |
  DDIV            |
  IREM            |
  LREM            |
  FREM            |
  DREM            |
  INEG            |
  LNEG            |
  FNEG            |
  DNEG            |
  ISHL            |
  LSHL            |
  ISHR            |
  LSHR            |
  IUSHR           |
  LUSHR           |
  IAND            |
  LAND            |
  IOR             |
  LOR             |
  IXOR            |
  LXOR            |
  IINC            |
  I2L             |
  I2F             |
  I2D             |
  L2I             |
  L2F             |
  L2D             |
  F2I             |
  F2L             |
  F2D             |
  D2I             |
  D2L             |
  D2F             |
  I2B             |
  I2C             |
  I2S             |
  LCMP            |
  FCMPL           |
  FCMPG           |
  DCMPL           |
  DCMPG           |
  IFEQ            |
  IFNE            |
  IFLT            |
  IFGE            |
  IFGT            |
  IFLE            |
  IF_ICMPEQ       |
  IF_ICMPNE       |
  IF_ICMPLT       |
  IF_ICMPGE       |
  IF_ICMPGT       |
  IF_ICMPLE       |
  IF_ACMPEQ       |
  IF_ACMPNE       |
  GOTO            |
  JSR             |
  RET             |
  TABLESWITCH     |
  LOOKUPSWITCH    |
  IRETURN         |
  LRETURN         |
  FRETURN         |
  DRETURN         |
  ARETURN         |
  RETURN          |
  GETSTATIC       |
  PUTSTATIC       |
  GETFIELD        |
  PUTFIELD        |
  INVOKEVIRTUAL   |
  INVOKESPECIAL   |
  INVOKESTATIC    |
  INVOKEINTERFACE |
  INVOKEDYNAMIC   |
  NEW             |
  NEWARRAY        |
  ANEWARRAY       |
  ARRAYLENGTH     |
  ATHROW          |
  CHECKCAST       |
  INSTANCEOF      |
  MONITORENTER    |
  MONITOREXIT     |
  WIDE            |
  MULTIANEWARRAY  |
  IFNULL          |
  IFNONNULL       |
  GOTO_W          |
  JSR_W           |
  BREAKPOINT      |
  IMPDEP1         |
  IMPDEP2
  deriving Show

instance Binary Opcode where
  get = do
    opcode <- getTag
    case opcode of
      0   -> return NOP
      1   -> return ACONST_NULL
      2   -> return ICONST_M1
      3   -> return ICONST_0
      4   -> return ICONST_1
      5   -> return ICONST_2
      6   -> return ICONST_3
      7   -> return ICONST_4
      8   -> return ICONST_5
      9   -> return LCONST_0
      10  -> return LCONST_1
      11  -> return FCONST_0
      12  -> return FCONST_1
      13  -> return FCONST_2
      14  -> return DCONST_0
      15  -> return DCONST_1
      16  -> return BIPUSH
      17  -> return SIPUSH
      18  -> return LDC
      19  -> return LDC_W
      20  -> return LDC2_W
      21  -> return ILOAD
      22  -> return LLOAD
      23  -> return FLOAD
      24  -> return DLOAD
      25  -> return ALOAD
      26  -> return ILOAD_0
      27  -> return ILOAD_1
      28  -> return ILOAD_2
      29  -> return ILOAD_3
      30  -> return LLOAD_0
      31  -> return LLOAD_1
      32  -> return LLOAD_2
      33  -> return LLOAD_3
      34  -> return FLOAD_0
      35  -> return FLOAD_1
      36  -> return FLOAD_2
      37  -> return FLOAD_3
      38  -> return DLOAD_0
      39  -> return DLOAD_1
      40  -> return DLOAD_2
      41  -> return DLOAD_3
      42  -> return ALOAD_0
      43  -> return ALOAD_1
      44  -> return ALOAD_2
      45  -> return ALOAD_3
      46  -> return IALOAD
      47  -> return LALOAD
      48  -> return FALOAD
      49  -> return DALOAD
      50  -> return AALOAD
      51  -> return BALOAD
      52  -> return CALOAD
      53  -> return SALOAD
      54  -> return ISTORE
      55  -> return LSTORE
      56  -> return FSTORE
      57  -> return DSTORE
      58  -> return ASTORE
      59  -> return ISTORE_0
      60  -> return ISTORE_1
      61  -> return ISTORE_2
      62  -> return ISTORE_3
      63  -> return LSTORE_0
      64  -> return LSTORE_1
      65  -> return LSTORE_2
      66  -> return LSTORE_3
      67  -> return FSTORE_0
      68  -> return FSTORE_1
      69  -> return FSTORE_2
      70  -> return FSTORE_3
      71  -> return DSTORE_0
      72  -> return DSTORE_1
      73  -> return DSTORE_2
      74  -> return DSTORE_3
      75  -> return ASTORE_0
      76  -> return ASTORE_1
      77  -> return ASTORE_2
      78  -> return ASTORE_3
      79  -> return IASTORE
      80  -> return LASTORE
      81  -> return FASTORE
      82  -> return DASTORE
      83  -> return AASTORE
      84  -> return BASTORE
      85  -> return CASTORE
      86  -> return SASTORE
      87  -> return POP
      88  -> return POP2
      89  -> return DUP
      90  -> return DUP_X1
      91  -> return DUP_X2
      92  -> return DUP2
      93  -> return DUP2_X1
      94  -> return DUP2_X2
      95  -> return SWAP
      96  -> return IADD
      97  -> return LADD
      98  -> return FADD
      99  -> return DADD
      100 -> return ISUB
      101 -> return LSUB
      102 -> return FSUB
      103 -> return DSUB
      104 -> return IMUL
      105 -> return LMUL
      106 -> return FMUL
      107 -> return DMUL
      108 -> return IDIV
      109 -> return LDIV
      110 -> return FDIV
      111 -> return DDIV
      112 -> return IREM
      113 -> return LREM
      114 -> return FREM
      115 -> return DREM
      116 -> return INEG
      117 -> return LNEG
      118 -> return FNEG
      119 -> return DNEG
      120 -> return ISHL
      121 -> return LSHL
      122 -> return ISHR
      123 -> return LSHR
      124 -> return IUSHR
      125 -> return LUSHR
      126 -> return IAND
      127 -> return LAND
      128 -> return IOR
      129 -> return LOR
      130 -> return IXOR
      131 -> return LXOR
      132 -> return IINC
      133 -> return I2L
      134 -> return I2F
      135 -> return I2D
      136 -> return L2I
      137 -> return L2F
      138 -> return L2D
      139 -> return F2I
      140 -> return F2L
      141 -> return F2D
      142 -> return D2I
      143 -> return D2L
      144 -> return D2F
      145 -> return I2B
      146 -> return I2C
      147 -> return I2S
      148 -> return LCMP
      149 -> return FCMPL
      150 -> return FCMPG
      151 -> return DCMPL
      152 -> return DCMPG
      153 -> return IFEQ
      154 -> return IFNE
      155 -> return IFLT
      156 -> return IFGE
      157 -> return IFGT
      158 -> return IFLE
      159 -> return IF_ICMPEQ
      160 -> return IF_ICMPNE
      161 -> return IF_ICMPLT
      162 -> return IF_ICMPGE
      163 -> return IF_ICMPGT
      164 -> return IF_ICMPLE
      165 -> return IF_ACMPEQ
      166 -> return IF_ACMPNE
      167 -> return GOTO
      168 -> return JSR
      169 -> return RET
      170 -> return TABLESWITCH
      171 -> return LOOKUPSWITCH
      172 -> return IRETURN
      173 -> return LRETURN
      174 -> return FRETURN
      175 -> return DRETURN
      176 -> return ARETURN
      177 -> return RETURN
      178 -> return GETSTATIC
      179 -> return PUTSTATIC
      180 -> return GETFIELD
      181 -> return PUTFIELD
      182 -> return INVOKEVIRTUAL
      183 -> return INVOKESPECIAL
      184 -> return INVOKESTATIC
      185 -> return INVOKEINTERFACE
      186 -> return INVOKEDYNAMIC
      187 -> return NEW
      188 -> return NEWARRAY
      189 -> return ANEWARRAY
      190 -> return ARRAYLENGTH
      191 -> return ATHROW
      192 -> return CHECKCAST
      193 -> return INSTANCEOF
      194 -> return MONITORENTER
      195 -> return MONITOREXIT
      196 -> return WIDE
      197 -> return MULTIANEWARRAY
      198 -> return IFNULL
      199 -> return IFNONNULL
      200 -> return GOTO_W
      201 -> return JSR_W
      202 -> return BREAKPOINT
      254 -> return IMPDEP1
      255 -> return IMPDEP2
      _   -> fail "invalid opcode"

  put opcode =
    putTag $ case opcode of
      NOP             -> 0
      ACONST_NULL     -> 1
      ICONST_M1       -> 2
      ICONST_0        -> 3
      ICONST_1        -> 4
      ICONST_2        -> 5
      ICONST_3        -> 6
      ICONST_4        -> 7
      ICONST_5        -> 8
      LCONST_0        -> 9
      LCONST_1        -> 10
      FCONST_0        -> 11
      FCONST_1        -> 12
      FCONST_2        -> 13
      DCONST_0        -> 14
      DCONST_1        -> 15
      BIPUSH          -> 16
      SIPUSH          -> 17
      LDC             -> 18
      LDC_W           -> 19
      LDC2_W          -> 20
      ILOAD           -> 21
      LLOAD           -> 22
      FLOAD           -> 23
      DLOAD           -> 24
      ALOAD           -> 25
      ILOAD_0         -> 26
      ILOAD_1         -> 27
      ILOAD_2         -> 28
      ILOAD_3         -> 29
      LLOAD_0         -> 30
      LLOAD_1         -> 31
      LLOAD_2         -> 32
      LLOAD_3         -> 33
      FLOAD_0         -> 34
      FLOAD_1         -> 35
      FLOAD_2         -> 36
      FLOAD_3         -> 37
      DLOAD_0         -> 38
      DLOAD_1         -> 39
      DLOAD_2         -> 40
      DLOAD_3         -> 41
      ALOAD_0         -> 42
      ALOAD_1         -> 43
      ALOAD_2         -> 44
      ALOAD_3         -> 45
      IALOAD          -> 46
      LALOAD          -> 47
      FALOAD          -> 48
      DALOAD          -> 49
      AALOAD          -> 50
      BALOAD          -> 51
      CALOAD          -> 52
      SALOAD          -> 53
      ISTORE          -> 54
      LSTORE          -> 55
      FSTORE          -> 56
      DSTORE          -> 57
      ASTORE          -> 58
      ISTORE_0        -> 59
      ISTORE_1        -> 60
      ISTORE_2        -> 61
      ISTORE_3        -> 62
      LSTORE_0        -> 63
      LSTORE_1        -> 64
      LSTORE_2        -> 65
      LSTORE_3        -> 66
      FSTORE_0        -> 67
      FSTORE_1        -> 68
      FSTORE_2        -> 69
      FSTORE_3        -> 70
      DSTORE_0        -> 71
      DSTORE_1        -> 72
      DSTORE_2        -> 73
      DSTORE_3        -> 74
      ASTORE_0        -> 75
      ASTORE_1        -> 76
      ASTORE_2        -> 77
      ASTORE_3        -> 78
      IASTORE         -> 79
      LASTORE         -> 80
      FASTORE         -> 81
      DASTORE         -> 82
      AASTORE         -> 83
      BASTORE         -> 84
      CASTORE         -> 85
      SASTORE         -> 86
      POP             -> 87
      POP2            -> 88
      DUP             -> 89
      DUP_X1          -> 90
      DUP_X2          -> 91
      DUP2            -> 92
      DUP2_X1         -> 93
      DUP2_X2         -> 94
      SWAP            -> 95
      IADD            -> 96
      LADD            -> 97
      FADD            -> 98
      DADD            -> 99
      ISUB            -> 100
      LSUB            -> 101
      FSUB            -> 102
      DSUB            -> 103
      IMUL            -> 104
      LMUL            -> 105
      FMUL            -> 106
      DMUL            -> 107
      IDIV            -> 108
      LDIV            -> 109
      FDIV            -> 110
      DDIV            -> 111
      IREM            -> 112
      LREM            -> 113
      FREM            -> 114
      DREM            -> 115
      INEG            -> 116
      LNEG            -> 117
      FNEG            -> 118
      DNEG            -> 119
      ISHL            -> 120
      LSHL            -> 121
      ISHR            -> 122
      LSHR            -> 123
      IUSHR           -> 124
      LUSHR           -> 125
      IAND            -> 126
      LAND            -> 127
      IOR             -> 128
      LOR             -> 129
      IXOR            -> 130
      LXOR            -> 131
      IINC            -> 132
      I2L             -> 133
      I2F             -> 134
      I2D             -> 135
      L2I             -> 136
      L2F             -> 137
      L2D             -> 138
      F2I             -> 139
      F2L             -> 140
      F2D             -> 141
      D2I             -> 142
      D2L             -> 143
      D2F             -> 144
      I2B             -> 145
      I2C             -> 146
      I2S             -> 147
      LCMP            -> 148
      FCMPL           -> 149
      FCMPG           -> 150
      DCMPL           -> 151
      DCMPG           -> 152
      IFEQ            -> 153
      IFNE            -> 154
      IFLT            -> 155
      IFGE            -> 156
      IFGT            -> 157
      IFLE            -> 158
      IF_ICMPEQ       -> 159
      IF_ICMPNE       -> 160
      IF_ICMPLT       -> 161
      IF_ICMPGE       -> 162
      IF_ICMPGT       -> 163
      IF_ICMPLE       -> 164
      IF_ACMPEQ       -> 165
      IF_ACMPNE       -> 166
      GOTO            -> 167
      JSR             -> 168
      RET             -> 169
      TABLESWITCH     -> 170
      LOOKUPSWITCH    -> 171
      IRETURN         -> 172
      LRETURN         -> 173
      FRETURN         -> 174
      DRETURN         -> 175
      ARETURN         -> 176
      RETURN          -> 177
      GETSTATIC       -> 178
      PUTSTATIC       -> 179
      GETFIELD        -> 180
      PUTFIELD        -> 181
      INVOKEVIRTUAL   -> 182
      INVOKESPECIAL   -> 183
      INVOKESTATIC    -> 184
      INVOKEINTERFACE -> 185
      INVOKEDYNAMIC   -> 186
      NEW             -> 187
      NEWARRAY        -> 188
      ANEWARRAY       -> 189
      ARRAYLENGTH     -> 190
      ATHROW          -> 191
      CHECKCAST       -> 192
      INSTANCEOF      -> 193
      MONITORENTER    -> 194
      MONITOREXIT     -> 195
      WIDE            -> 196
      MULTIANEWARRAY  -> 197
      IFNULL          -> 198
      IFNONNULL       -> 199
      GOTO_W          -> 200
      JSR_W           -> 201
      BREAKPOINT      -> 202
      IMPDEP1         -> 254
      IMPDEP2         -> 255