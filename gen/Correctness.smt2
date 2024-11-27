;-------------------------------------------------------------------------------
; Defining Tuple sorts
(declare-datatypes
 ((Tuple2 2) (Tuple3 3))
 ((par (X1 X2) ((mkTuple2 (tpl2_1 X1) (tpl2_2 X2))))
  (par (X1 X2 X3) ((mkTuple3 (tpl3_1 X1) (tpl3_2 X2) (tpl3_3 X3))))))
; Defining a generic ListX sort
(declare-datatype ListX (par (X) ((nil) (cons (head X) (tail (ListX X))))))
; Defining an "and" reduction function for ListX Bool
(define-fun-rec
 andReduce
 ((lst (ListX Bool)))
 Bool
 (match lst ((nil true) ((cons h t) (and h (andReduce t))))))
; Defining an "implies" reduction function for ListX Bool
(define-fun-rec
 impliesReduce
 ((lst (ListX Bool)))
 Bool
 (match
  lst
  ((nil true)
   ((cons h t) (match t ((nil h) ((cons hh tt) (=> h (impliesReduce t)))))))))
;-------------------------------------------------------------------------------
; code gen for "assert_v_258"
;-------------------------------------------------------------------------------
(push)
;-------------------------------------------------------------------------------
; Defining the specific Inputs record type
(declare-datatype
 Input_assert_v_258
 ((mkInput_assert_v_258
   (vinpt_imem_peek_mask_11 (_ BitVec 1))
   (vinpt_rs1_valid_14 (_ BitVec 1))
   (vinpt_rs1_22 (_ BitVec 2))
   (vinpt_mreq_val_72 (_ BitVec 3))
   (vinpt_dmem_put_mask_86 (_ BitVec 1))
   (vinpt_dmem_peek_mask_104 (_ BitVec 1))
   (vinpt_rs2_valid_119 (_ BitVec 1))
   (vinpt_rs2_120 (_ BitVec 2))
   (vinpt_imem_put_mask_163 (_ BitVec 1))
   (vinpt_branch_valid_174 (_ BitVec 1))
   (vinpt_branch_target_179 (_ BitVec 3))
   (vinpt_predicted_pc_199 (_ BitVec 3))
   (vinpt_rd_valid_204 (_ BitVec 1))
   (vinpt_rd_205 (_ BitVec 2))
   (vinpt_is_mem_access_211 (_ BitVec 1))
   (vinpt_can_branch_212 (_ BitVec 1)))))
;-------------------------------------------------------------------------------
; Defining the specific State record type
(declare-datatype
 State_assert_v_258
 ((mkState_assert_v_258
   (vreg_5 (_ BitVec 1))
   (vreg_42 (_ BitVec 1))
   (vreg_60 (_ BitVec 14))
   (vreg_75 (_ BitVec 4))
   (vreg_103 (_ BitVec 1))
   (vreg_146 (_ BitVec 1))
   (vreg_160 (_ BitVec 1))
   (vreg_170 (_ BitVec 3))
   (vreg_172 (_ BitVec 3))
   (vreg_183 (_ BitVec 3))
   (vreg_197 (_ BitVec 1))
   (vreg_203 (_ BitVec 3))
   (vreg_217 (_ BitVec 14))
   (vreg_235 (_ BitVec 14))
   (vreg_252 (_ BitVec 1)))))
;-------------------------------------------------------------------------------
; Defining the specific transition function
(define-fun
 tFun_assert_v_258
 ((inpts Input_assert_v_258) (prev State_assert_v_258))
 (Tuple2 Bool State_assert_v_258)
 (let ((v_1 (bvnot ((_ int2bv 1) 1))))
 (let ((v_236 ((_ extract 4 0) (vreg_235 prev))))
 (let ((v_237 ((_ extract 1 0) v_236)))
 (let ((v_238 ((_ extract 1 1) v_237)))
 (let ((v_77 (bvnot (vreg_103 prev))))
 (let ((v_61 ((_ extract 4 0) (vreg_60 prev))))
 (let ((v_62 ((_ extract 1 0) v_61)))
 (let ((v_63 ((_ extract 1 1) v_62)))
 (let ((v_64 ((_ extract 13 5) (vreg_60 prev))))
 (let ((v_65 ((_ extract 5 0) v_64)))
 (let ((v_66 ((_ extract 5 3) v_65)))
 (let ((v_67 ((_ extract 2 2) v_66)))
 (let ((v_105 (bvand (vreg_103 prev) (vinpt_dmem_peek_mask_104 inpts))))
 (let ((v_106 (bvnot v_105)))
 (let ((v_107 (bvand v_67 v_106)))
 (let ((v_108 (bvand v_63 v_107)))
 (let ((v_78 (bvnot v_108)))
 (let ((v_79 (bvand v_67 v_78)))
 (let ((v_80 (bvand (vreg_42 prev) v_79)))
 (let ((v_81 (bvand v_80 ((_ int2bv 1) 1))))
 (let ((v_82 (bvand v_63 v_81)))
 (let ((v_83 (bvnot v_82)))
 (let ((v_84
        (bvor
         (ite (= v_83 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_82 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_85 (bvor v_77 v_84)))
 (let ((v_87 (bvand v_85 (vinpt_dmem_put_mask_86 inpts))))
 (let ((v_239 (bvnot v_87)))
 (let ((v_240 (bvand v_238 v_239)))
 (let ((v_36 (bvnot ((_ int2bv 1) 1))))
 (let ((v_109 (bvand (vreg_42 prev) v_108)))
 (let ((v_110
        (bvor
         (ite (= v_36 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_109 ((_ int2bv 1) 0)))))
 (let ((v_241 (bvor v_240 v_110)))
 (let ((v_242 (bvand (vreg_5 prev) v_241)))
 (let ((v_243
        (bvor
         (ite (= v_1 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_242 ((_ int2bv 1) 0)))))
 (let ((v_253 (bvnot v_243)))
 (let ((v_184 (bvnot (ite (= (vreg_172 prev) (vreg_183 prev)) #b1 #b0))))
 (let ((v_254 (bvnot v_184)))
 (let ((v_255 (bvand v_253 v_254)))
 (let ((v_256 (bvand (vreg_252 prev) v_255)))
 (let ((v_257 (bvand v_256 ((_ int2bv 1) 1))))
 (let ((v_218 ((_ extract 13 5) (vreg_217 prev))))
 (let ((v_219 ((_ extract 8 6) v_218)))
 (let ((v_258 (ite (= v_219 (vreg_172 prev)) #b1 #b0)))
 (let ((v_0 (bvnot ((_ int2bv 1) 1))))
 (let ((v_244 (bvand (vreg_252 prev) v_243)))
 (let ((v_245
        (bvor
         (ite (= v_0 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_244 ((_ int2bv 1) 0)))))
 (let ((v_246 (bvnot v_245)))
 (let ((v_247 (bvand v_246 ((_ int2bv 1) 1))))
 (let ((v_206 (concat (vinpt_rd_valid_204 inpts) (vinpt_rd_205 inpts))))
 (let ((v_207 (concat (vinpt_rs1_valid_14 inpts) (vinpt_rs1_22 inpts))))
 (let ((v_208 (concat v_206 v_207)))
 (let ((v_209 (concat (vreg_203 prev) v_208)))
 (let ((v_210 (concat (vinpt_rs2_valid_119 inpts) (vinpt_rs2_120 inpts))))
 (let ((v_213
        (concat (vinpt_is_mem_access_211 inpts) (vinpt_can_branch_212 inpts))))
 (let ((v_214 (concat v_210 v_213)))
 (let ((v_215 (concat v_209 v_214)))
 (let ((v_216 (bvor (ite (= v_247 #b1) v_215 ((_ int2bv 14) 0)))))
 (let ((v_161 (bvnot (vreg_160 prev))))
 (let ((v_12 (bvand (vreg_160 prev) (vinpt_imem_peek_mask_11 inpts))))
 (let ((v_13 (bvnot v_245)))
 (let ((v_15 ((_ extract 4 0) (vreg_217 prev))))
 (let ((v_16 ((_ extract 1 0) v_15)))
 (let ((v_17 ((_ extract 1 1) v_16)))
 (let ((v_18 ((_ extract 5 0) v_218)))
 (let ((v_19 ((_ extract 5 3) v_18)))
 (let ((v_20 ((_ extract 2 2) v_19)))
 (let ((v_21 ((_ extract 1 0) v_19)))
 (let ((v_23 (ite (= v_21 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_24 (bvand v_20 v_23)))
 (let ((v_25 (bvand v_17 v_24)))
 (let ((v_26 (bvand (vreg_252 prev) v_25)))
 (let ((v_27 ((_ extract 13 5) (vreg_235 prev))))
 (let ((v_28 ((_ extract 5 0) v_27)))
 (let ((v_29 ((_ extract 5 3) v_28)))
 (let ((v_30 ((_ extract 2 2) v_29)))
 (let ((v_31 ((_ extract 1 0) v_29)))
 (let ((v_32 (ite (= v_31 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_33 (bvand v_30 v_32)))
 (let ((v_34 (bvand v_238 v_33)))
 (let ((v_35 (bvand (vreg_5 prev) v_34)))
 (let ((v_111 ((_ extract 1 0) v_66)))
 (let ((v_112 (ite (= v_111 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_113 (bvand v_67 v_112)))
 (let ((v_114 (bvand v_63 v_113)))
 (let ((v_115 (bvand v_110 v_114)))
 (let ((v_116 (bvor v_35 v_115)))
 (let ((v_117 (bvor v_26 v_116)))
 (let ((v_118 (bvand (vinpt_rs1_valid_14 inpts) v_117)))
 (let ((v_121 (ite (= v_21 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_122 (bvand v_20 v_121)))
 (let ((v_123 (bvand v_17 v_122)))
 (let ((v_124 (bvand (vreg_252 prev) v_123)))
 (let ((v_125 (ite (= v_31 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_126 (bvand v_30 v_125)))
 (let ((v_127 (bvand v_238 v_126)))
 (let ((v_128 (bvand (vreg_5 prev) v_127)))
 (let ((v_129 (ite (= v_111 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_130 (bvand v_67 v_129)))
 (let ((v_131 (bvand v_63 v_130)))
 (let ((v_132 (bvand v_110 v_131)))
 (let ((v_133 (bvor v_128 v_132)))
 (let ((v_134 (bvor v_124 v_133)))
 (let ((v_135 (bvand (vinpt_rs2_valid_119 inpts) v_134)))
 (let ((v_136 (bvor v_118 v_135)))
 (let ((v_137 (bvnot v_136)))
 (let ((v_138 (bvand v_13 v_137)))
 (let ((v_139 (bvand v_12 v_138)))
 (let ((v_147 (bvand (vreg_146 prev) v_139)))
 (let ((v_148 (bvand v_147 ((_ int2bv 1) 1))))
 (let ((v_149 (bvnot v_148)))
 (let ((v_150
        (bvor
         (ite (= v_149 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_148 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_162 (bvor v_161 v_150)))
 (let ((v_164 (bvand v_162 (vinpt_imem_put_mask_163 inpts))))
 (let ((v_10 (bvnot ((_ int2bv 1) 1))))
 (let ((v_140 (bvnot v_139)))
 (let ((v_141 (bvand (vreg_146 prev) v_140)))
 (let ((v_142
        (bvor
         (ite (= v_10 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_141 ((_ int2bv 1) 0)))))
 (let ((v_143 (bvnot v_142)))
 (let ((v_144 (bvand v_143 ((_ int2bv 1) 1))))
 (let ((v_165 (bvand v_164 v_144)))
 (let ((act_166 (bvand ((_ int2bv 1) 1) v_165)))
 (let ((v_6 (bvnot ((_ int2bv 1) 0))))
 (let ((v_7 (bvor (ite (= v_6 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_154 (bvnot v_7)))
 (let ((v_155 (bvand ((_ int2bv 1) 1) v_154)))
 (let ((v_167 (bvand act_166 v_155)))
 (let ((v_145 (bvor (ite (= v_144 #b1) v_164 ((_ int2bv 1) 0)))))
 (let ((v_8 (bvand ((_ int2bv 1) 1) v_7)))
 (let ((v_9 (bvnot act_166)))
 (let ((v_151 (bvnot (vreg_160 prev))))
 (let ((v_152 (bvor v_150 v_151)))
 (let ((v_153 (bvand v_9 v_152)))
 (let ((v_156 (bvand v_153 v_155)))
 (let ((v_157 (bvor v_167 v_156)))
 (let ((v_158 (bvor v_8 v_157)))
 (let ((v_159
        (bvor
         (ite (= v_167 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_156 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_8 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_168 (bvnot act_166)))
 (let ((v_185 (bvand (vreg_252 prev) v_184)))
 (let ((v_186 (bvand v_185 ((_ int2bv 1) 1))))
 (let ((v_187 (bvnot v_186)))
 (let ((v_188
        (bvor
         (ite (= v_187 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_186 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_198 (bvor v_188 (vreg_197 prev))))
 (let ((v_200
        (ite
         (= v_198 ((_ int2bv 1) 0))
         (vinpt_predicted_pc_199 inpts)
         (vreg_183 prev))))
 (let ((v_201
        (bvor
         (ite (= v_168 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite (= act_166 #b1) v_200 ((_ int2bv 3) 0)))))
 (let ((v_202 (bvor (ite (= v_167 #b1) v_201 ((_ int2bv 3) 0)))))
 (let ((v_189 (bvnot v_165)))
 (let ((v_190
        (bvor
         (ite (= v_189 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_165 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_191 (bvnot v_190)))
 (let ((v_192 (bvand ((_ int2bv 1) 1) v_191)))
 (let ((v_193 (bvand v_188 v_192)))
 (let ((v_194 (bvand ((_ int2bv 1) 1) v_190)))
 (let ((v_195 (bvor v_193 v_194)))
 (let ((v_196
        (bvor
         (ite (= v_193 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_194 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_173 ((_ extract 0 0) v_16)))
 (let ((v_175 (bvand v_173 (vinpt_branch_valid_174 inpts))))
 (let ((act_176 (bvand v_175 v_257)))
 (let ((v_177 (bvadd (vreg_172 prev) ((_ int2bv 3) 1))))
 (let ((v_178 (bvnot act_176)))
 (let ((v_180
        (bvor
         (ite (= v_178 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite
          (= act_176 #b1)
          (vinpt_branch_target_179 inpts)
          ((_ int2bv 3) 0)))))
 (let ((v_181 (ite (= act_176 ((_ int2bv 1) 0)) v_177 v_180)))
 (let ((v_182 (bvor (ite (= v_257 #b1) v_181 ((_ int2bv 3) 0)))))
 (let ((v_171 (bvor (ite (= v_247 #b1) (vreg_170 prev) ((_ int2bv 3) 0)))))
 (let ((v_169 (bvor (ite (= v_144 #b1) v_200 ((_ int2bv 3) 0)))))
 (let ((v_37 (bvnot v_110)))
 (let ((v_38 (bvand v_37 ((_ int2bv 1) 1))))
 (let ((v_39 (bvnot v_240)))
 (let ((v_40 (bvand (vreg_5 prev) v_39)))
 (let ((v_41 (bvor (ite (= v_38 #b1) v_40 ((_ int2bv 1) 0)))))
 (let ((v_43 ((_ extract 8 6) v_27)))
 (let ((v_44 (concat v_30 v_31)))
 (let ((v_45 ((_ extract 2 0) v_28)))
 (let ((v_46 ((_ extract 2 2) v_45)))
 (let ((v_47 ((_ extract 1 0) v_45)))
 (let ((v_48 (concat v_46 v_47)))
 (let ((v_49 (concat v_44 v_48)))
 (let ((v_50 (concat v_43 v_49)))
 (let ((v_51 ((_ extract 4 2) v_236)))
 (let ((v_52 ((_ extract 2 2) v_51)))
 (let ((v_53 ((_ extract 1 0) v_51)))
 (let ((v_54 (concat v_52 v_53)))
 (let ((v_55 ((_ extract 0 0) v_237)))
 (let ((v_56 (concat v_238 v_55)))
 (let ((v_57 (concat v_54 v_56)))
 (let ((v_58 (concat v_50 v_57)))
 (let ((v_59 (bvor (ite (= v_38 #b1) v_58 ((_ int2bv 14) 0)))))
 (let ((v_68 (bvnot ((_ int2bv 1) 0))))
 (let ((v_69 (bvor (ite (= v_68 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_70 (bvand ((_ int2bv 1) 1) v_69)))
 (let ((v_76 ((_ extract 0 0) (vreg_75 prev))))
 (let ((v_88 (bvand v_238 v_87)))
 (let ((v_89 (bvand (vreg_5 prev) v_88)))
 (let ((v_90 (bvand v_89 v_38)))
 (let ((act_91 (bvand v_76 v_90)))
 (let ((v_92 (bvnot v_69)))
 (let ((v_93 (bvand ((_ int2bv 1) 1) v_92)))
 (let ((v_94 (bvand act_91 v_93)))
 (let ((v_95 (bvnot act_91)))
 (let ((v_96 (bvnot (vreg_103 prev))))
 (let ((v_97 (bvor v_84 v_96)))
 (let ((v_98 (bvand v_95 v_97)))
 (let ((v_99 (bvand v_98 v_93)))
 (let ((v_100 (bvor v_94 v_99)))
 (let ((v_101 (bvor v_70 v_100)))
 (let ((v_71 (bvand v_17 v_257)))
 (let ((v_73 (concat (vinpt_mreq_val_72 inpts) v_20)))
 (let ((v_74 (bvor (ite (= v_71 #b1) v_73 ((_ int2bv 4) 0)))))
 (let ((v_102
        (bvor
         (ite (= v_94 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_99 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_70 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_2 (bvnot v_243)))
 (let ((v_3 (bvand v_2 ((_ int2bv 1) 1))))
 (let ((v_220 (concat v_20 v_21)))
 (let ((v_221 ((_ extract 2 0) v_18)))
 (let ((v_222 ((_ extract 2 2) v_221)))
 (let ((v_223 ((_ extract 1 0) v_221)))
 (let ((v_224 (concat v_222 v_223)))
 (let ((v_225 (concat v_220 v_224)))
 (let ((v_226 (concat v_219 v_225)))
 (let ((v_227 ((_ extract 4 2) v_15)))
 (let ((v_228 ((_ extract 2 2) v_227)))
 (let ((v_229 ((_ extract 1 0) v_227)))
 (let ((v_230 (concat v_228 v_229)))
 (let ((v_231 (concat v_17 v_173)))
 (let ((v_232 (concat v_230 v_231)))
 (let ((v_233 (concat v_226 v_232)))
 (let ((v_234 (bvor (ite (= v_3 #b1) v_233 ((_ int2bv 14) 0)))))
 (let ((v_4 (bvor (ite (= v_3 #b1) v_256 ((_ int2bv 1) 0)))))
 (let ((v_248 (bvnot v_198)))
 (let ((v_249 (bvand v_139 v_248)))
 (let ((v_250 (bvand (vreg_146 prev) v_249)))
 (let ((v_251 (bvor (ite (= v_247 #b1) v_250 ((_ int2bv 1) 0)))))
 (mkTuple2
  (=> (= v_257 #b1) (= v_258 #b1))
  (mkState_assert_v_258
   (ite (= v_3 #b1) v_4 (vreg_5 prev))
   (ite (= v_38 #b1) v_41 (vreg_42 prev))
   (ite (= v_38 #b1) v_59 (vreg_60 prev))
   (ite (= v_71 #b1) v_74 (vreg_75 prev))
   (ite (= v_101 #b1) v_102 (vreg_103 prev))
   (ite (= v_144 #b1) v_145 (vreg_146 prev))
   (ite (= v_158 #b1) v_159 (vreg_160 prev))
   (ite (= v_144 #b1) v_169 (vreg_170 prev))
   (ite (= v_247 #b1) v_171 (vreg_172 prev))
   (ite (= v_257 #b1) v_182 (vreg_183 prev))
   (ite (= v_195 #b1) v_196 (vreg_197 prev))
   (ite (= v_167 #b1) v_202 (vreg_203 prev))
   (ite (= v_247 #b1) v_216 (vreg_217 prev))
   (ite (= v_3 #b1) v_234 (vreg_235 prev))
   (ite
    (= v_247 #b1)
    v_251
    (vreg_252
     prev)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;-------------------------------------------------------------------------------
; Defining the specific chaining of the transition function
(define-fun-rec
 chain_tFun_assert_v_258
 ((xs (ListX Input_assert_v_258)) (prev State_assert_v_258))
 (Tuple2 (ListX Bool) (ListX State_assert_v_258))
 (match
  xs
  ((nil
    (mkTuple2
     (as nil (ListX Bool))
     (cons prev (as nil (ListX State_assert_v_258)))))
   ((cons h t)
    (match
     (tFun_assert_v_258 h prev)
     (((mkTuple2 ret next)
       (match
        (chain_tFun_assert_v_258 t next)
        (((mkTuple2 rets ys) (mkTuple2 (cons ret rets) (cons prev ys))))))))))))
(push)
;-------------------------------------------------------------------------------
(echo "Instruction correct")
(echo "--------------------------------------------------------------------------------")
(echo "Using bounded verification of depth 7")
(echo "Bounded property refutation:")
(declare-const in0 Input_assert_v_258)
(declare-const in1 Input_assert_v_258)
(declare-const in2 Input_assert_v_258)
(declare-const in3 Input_assert_v_258)
(declare-const in4 Input_assert_v_258)
(declare-const in5 Input_assert_v_258)
(declare-const in6 Input_assert_v_258)
(assert
 (let ((s0
        (mkState_assert_v_258
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 4) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 1) 0)))
       (inpts
        (cons
         in0
         (cons
          in1
          (cons
           in2
           (cons
            in3
            (cons
             in4
             (cons in5 (cons in6 (as nil (ListX Input_assert_v_258)))))))))))
      (match
       (match
        (tFun_assert_v_258 in0 s0)
        (((mkTuple2 ret0 s1)
          (match
           (tFun_assert_v_258 in1 s1)
           (((mkTuple2 ret1 s2)
             (match
              (tFun_assert_v_258 in2 s2)
              (((mkTuple2 ret2 s3)
                (match
                 (tFun_assert_v_258 in3 s3)
                 (((mkTuple2 ret3 s4)
                   (match
                    (tFun_assert_v_258 in4 s4)
                    (((mkTuple2 ret4 s5)
                      (match
                       (tFun_assert_v_258 in5 s5)
                       (((mkTuple2 ret5 s6)
                         (match
                          (tFun_assert_v_258 in6 s6)
                          (((mkTuple2 ret6 s7)
                            (mkTuple2
                             (cons
                              ret0
                              (cons
                               ret1
                               (cons
                                ret2
                                (cons
                                 ret3
                                 (cons
                                  ret4
                                  (cons
                                   ret5
                                   (cons ret6 (as nil (ListX Bool)))))))))
                             (cons
                              s0
                              (cons
                               s1
                               (cons
                                s2
                                (cons
                                 s3
                                 (cons
                                  s4
                                  (cons
                                   s5
                                   (cons
                                    s6
                                    (as
                                     nil
                                     (ListX State_assert_v_258)))))))))))))))))))))))))))))))
       (((mkTuple2 oks ss) (not (andReduce oks)))))))
(check-sat)
(pop)
(echo "")
;-------------------------------------------------------------------------------
; code gen for "assert_v_264"
;-------------------------------------------------------------------------------
(push)
;-------------------------------------------------------------------------------
; Defining the specific Inputs record type
(declare-datatype
 Input_assert_v_264
 ((mkInput_assert_v_264
   (vinpt_imem_peek_mask_11 (_ BitVec 1))
   (vinpt_rs1_valid_14 (_ BitVec 1))
   (vinpt_rs1_22 (_ BitVec 2))
   (vinpt_mreq_val_72 (_ BitVec 3))
   (vinpt_dmem_put_mask_86 (_ BitVec 1))
   (vinpt_dmem_peek_mask_104 (_ BitVec 1))
   (vinpt_rs2_valid_119 (_ BitVec 1))
   (vinpt_rs2_120 (_ BitVec 2))
   (vinpt_imem_put_mask_163 (_ BitVec 1))
   (vinpt_branch_valid_174 (_ BitVec 1))
   (vinpt_branch_target_179 (_ BitVec 3))
   (vinpt_predicted_pc_199 (_ BitVec 3))
   (vinpt_rd_valid_204 (_ BitVec 1))
   (vinpt_rd_205 (_ BitVec 2))
   (vinpt_is_mem_access_211 (_ BitVec 1))
   (vinpt_can_branch_212 (_ BitVec 1)))))
;-------------------------------------------------------------------------------
; Defining the specific State record type
(declare-datatype
 State_assert_v_264
 ((mkState_assert_v_264
   (vreg_5 (_ BitVec 1))
   (vreg_42 (_ BitVec 1))
   (vreg_60 (_ BitVec 14))
   (vreg_75 (_ BitVec 4))
   (vreg_103 (_ BitVec 1))
   (vreg_146 (_ BitVec 1))
   (vreg_160 (_ BitVec 1))
   (vreg_170 (_ BitVec 3))
   (vreg_172 (_ BitVec 3))
   (vreg_183 (_ BitVec 3))
   (vreg_197 (_ BitVec 1))
   (vreg_203 (_ BitVec 3))
   (vreg_217 (_ BitVec 14))
   (vreg_235 (_ BitVec 14))
   (vreg_252 (_ BitVec 1))
   (vreg_263 (_ BitVec 3)))))
;-------------------------------------------------------------------------------
; Defining the specific transition function
(define-fun
 tFun_assert_v_264
 ((inpts Input_assert_v_264) (prev State_assert_v_264))
 (Tuple2 Bool State_assert_v_264)
 (let ((v_1 (bvnot ((_ int2bv 1) 1))))
 (let ((v_236 ((_ extract 4 0) (vreg_235 prev))))
 (let ((v_237 ((_ extract 1 0) v_236)))
 (let ((v_238 ((_ extract 1 1) v_237)))
 (let ((v_77 (bvnot (vreg_103 prev))))
 (let ((v_61 ((_ extract 4 0) (vreg_60 prev))))
 (let ((v_62 ((_ extract 1 0) v_61)))
 (let ((v_63 ((_ extract 1 1) v_62)))
 (let ((v_64 ((_ extract 13 5) (vreg_60 prev))))
 (let ((v_65 ((_ extract 5 0) v_64)))
 (let ((v_66 ((_ extract 5 3) v_65)))
 (let ((v_67 ((_ extract 2 2) v_66)))
 (let ((v_105 (bvand (vreg_103 prev) (vinpt_dmem_peek_mask_104 inpts))))
 (let ((v_106 (bvnot v_105)))
 (let ((v_107 (bvand v_67 v_106)))
 (let ((v_108 (bvand v_63 v_107)))
 (let ((v_78 (bvnot v_108)))
 (let ((v_79 (bvand v_67 v_78)))
 (let ((v_80 (bvand (vreg_42 prev) v_79)))
 (let ((v_81 (bvand v_80 ((_ int2bv 1) 1))))
 (let ((v_82 (bvand v_63 v_81)))
 (let ((v_83 (bvnot v_82)))
 (let ((v_84
        (bvor
         (ite (= v_83 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_82 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_85 (bvor v_77 v_84)))
 (let ((v_87 (bvand v_85 (vinpt_dmem_put_mask_86 inpts))))
 (let ((v_239 (bvnot v_87)))
 (let ((v_240 (bvand v_238 v_239)))
 (let ((v_36 (bvnot ((_ int2bv 1) 1))))
 (let ((v_109 (bvand (vreg_42 prev) v_108)))
 (let ((v_110
        (bvor
         (ite (= v_36 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_109 ((_ int2bv 1) 0)))))
 (let ((v_241 (bvor v_240 v_110)))
 (let ((v_242 (bvand (vreg_5 prev) v_241)))
 (let ((v_243
        (bvor
         (ite (= v_1 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_242 ((_ int2bv 1) 0)))))
 (let ((v_253 (bvnot v_243)))
 (let ((v_184 (bvnot (ite (= (vreg_172 prev) (vreg_183 prev)) #b1 #b0))))
 (let ((v_254 (bvnot v_184)))
 (let ((v_255 (bvand v_253 v_254)))
 (let ((v_256 (bvand (vreg_252 prev) v_255)))
 (let ((v_257 (bvand v_256 ((_ int2bv 1) 1))))
 (let ((v_264 (ite (= (vreg_172 prev) (vreg_263 prev)) #b1 #b0)))
 (let ((v_15 ((_ extract 4 0) (vreg_217 prev))))
 (let ((v_16 ((_ extract 1 0) v_15)))
 (let ((v_173 ((_ extract 0 0) v_16)))
 (let ((v_175 (bvand v_173 (vinpt_branch_valid_174 inpts))))
 (let ((v_260 (bvadd (vreg_263 prev) ((_ int2bv 3) 1))))
 (let ((v_261
        (ite (= v_175 ((_ int2bv 1) 0)) v_260 (vinpt_branch_target_179 inpts))))
 (let ((v_262 (bvor (ite (= v_257 #b1) v_261 ((_ int2bv 3) 0)))))
 (let ((v_0 (bvnot ((_ int2bv 1) 1))))
 (let ((v_244 (bvand (vreg_252 prev) v_243)))
 (let ((v_245
        (bvor
         (ite (= v_0 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_244 ((_ int2bv 1) 0)))))
 (let ((v_246 (bvnot v_245)))
 (let ((v_247 (bvand v_246 ((_ int2bv 1) 1))))
 (let ((v_206 (concat (vinpt_rd_valid_204 inpts) (vinpt_rd_205 inpts))))
 (let ((v_207 (concat (vinpt_rs1_valid_14 inpts) (vinpt_rs1_22 inpts))))
 (let ((v_208 (concat v_206 v_207)))
 (let ((v_209 (concat (vreg_203 prev) v_208)))
 (let ((v_210 (concat (vinpt_rs2_valid_119 inpts) (vinpt_rs2_120 inpts))))
 (let ((v_213
        (concat (vinpt_is_mem_access_211 inpts) (vinpt_can_branch_212 inpts))))
 (let ((v_214 (concat v_210 v_213)))
 (let ((v_215 (concat v_209 v_214)))
 (let ((v_216 (bvor (ite (= v_247 #b1) v_215 ((_ int2bv 14) 0)))))
 (let ((v_161 (bvnot (vreg_160 prev))))
 (let ((v_12 (bvand (vreg_160 prev) (vinpt_imem_peek_mask_11 inpts))))
 (let ((v_13 (bvnot v_245)))
 (let ((v_17 ((_ extract 1 1) v_16)))
 (let ((v_218 ((_ extract 13 5) (vreg_217 prev))))
 (let ((v_18 ((_ extract 5 0) v_218)))
 (let ((v_19 ((_ extract 5 3) v_18)))
 (let ((v_20 ((_ extract 2 2) v_19)))
 (let ((v_21 ((_ extract 1 0) v_19)))
 (let ((v_23 (ite (= v_21 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_24 (bvand v_20 v_23)))
 (let ((v_25 (bvand v_17 v_24)))
 (let ((v_26 (bvand (vreg_252 prev) v_25)))
 (let ((v_27 ((_ extract 13 5) (vreg_235 prev))))
 (let ((v_28 ((_ extract 5 0) v_27)))
 (let ((v_29 ((_ extract 5 3) v_28)))
 (let ((v_30 ((_ extract 2 2) v_29)))
 (let ((v_31 ((_ extract 1 0) v_29)))
 (let ((v_32 (ite (= v_31 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_33 (bvand v_30 v_32)))
 (let ((v_34 (bvand v_238 v_33)))
 (let ((v_35 (bvand (vreg_5 prev) v_34)))
 (let ((v_111 ((_ extract 1 0) v_66)))
 (let ((v_112 (ite (= v_111 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_113 (bvand v_67 v_112)))
 (let ((v_114 (bvand v_63 v_113)))
 (let ((v_115 (bvand v_110 v_114)))
 (let ((v_116 (bvor v_35 v_115)))
 (let ((v_117 (bvor v_26 v_116)))
 (let ((v_118 (bvand (vinpt_rs1_valid_14 inpts) v_117)))
 (let ((v_121 (ite (= v_21 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_122 (bvand v_20 v_121)))
 (let ((v_123 (bvand v_17 v_122)))
 (let ((v_124 (bvand (vreg_252 prev) v_123)))
 (let ((v_125 (ite (= v_31 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_126 (bvand v_30 v_125)))
 (let ((v_127 (bvand v_238 v_126)))
 (let ((v_128 (bvand (vreg_5 prev) v_127)))
 (let ((v_129 (ite (= v_111 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_130 (bvand v_67 v_129)))
 (let ((v_131 (bvand v_63 v_130)))
 (let ((v_132 (bvand v_110 v_131)))
 (let ((v_133 (bvor v_128 v_132)))
 (let ((v_134 (bvor v_124 v_133)))
 (let ((v_135 (bvand (vinpt_rs2_valid_119 inpts) v_134)))
 (let ((v_136 (bvor v_118 v_135)))
 (let ((v_137 (bvnot v_136)))
 (let ((v_138 (bvand v_13 v_137)))
 (let ((v_139 (bvand v_12 v_138)))
 (let ((v_147 (bvand (vreg_146 prev) v_139)))
 (let ((v_148 (bvand v_147 ((_ int2bv 1) 1))))
 (let ((v_149 (bvnot v_148)))
 (let ((v_150
        (bvor
         (ite (= v_149 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_148 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_162 (bvor v_161 v_150)))
 (let ((v_164 (bvand v_162 (vinpt_imem_put_mask_163 inpts))))
 (let ((v_10 (bvnot ((_ int2bv 1) 1))))
 (let ((v_140 (bvnot v_139)))
 (let ((v_141 (bvand (vreg_146 prev) v_140)))
 (let ((v_142
        (bvor
         (ite (= v_10 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_141 ((_ int2bv 1) 0)))))
 (let ((v_143 (bvnot v_142)))
 (let ((v_144 (bvand v_143 ((_ int2bv 1) 1))))
 (let ((v_165 (bvand v_164 v_144)))
 (let ((act_166 (bvand ((_ int2bv 1) 1) v_165)))
 (let ((v_6 (bvnot ((_ int2bv 1) 0))))
 (let ((v_7 (bvor (ite (= v_6 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_154 (bvnot v_7)))
 (let ((v_155 (bvand ((_ int2bv 1) 1) v_154)))
 (let ((v_167 (bvand act_166 v_155)))
 (let ((v_145 (bvor (ite (= v_144 #b1) v_164 ((_ int2bv 1) 0)))))
 (let ((v_8 (bvand ((_ int2bv 1) 1) v_7)))
 (let ((v_9 (bvnot act_166)))
 (let ((v_151 (bvnot (vreg_160 prev))))
 (let ((v_152 (bvor v_150 v_151)))
 (let ((v_153 (bvand v_9 v_152)))
 (let ((v_156 (bvand v_153 v_155)))
 (let ((v_157 (bvor v_167 v_156)))
 (let ((v_158 (bvor v_8 v_157)))
 (let ((v_159
        (bvor
         (ite (= v_167 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_156 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_8 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_168 (bvnot act_166)))
 (let ((v_185 (bvand (vreg_252 prev) v_184)))
 (let ((v_186 (bvand v_185 ((_ int2bv 1) 1))))
 (let ((v_187 (bvnot v_186)))
 (let ((v_188
        (bvor
         (ite (= v_187 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_186 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_198 (bvor v_188 (vreg_197 prev))))
 (let ((v_200
        (ite
         (= v_198 ((_ int2bv 1) 0))
         (vinpt_predicted_pc_199 inpts)
         (vreg_183 prev))))
 (let ((v_201
        (bvor
         (ite (= v_168 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite (= act_166 #b1) v_200 ((_ int2bv 3) 0)))))
 (let ((v_202 (bvor (ite (= v_167 #b1) v_201 ((_ int2bv 3) 0)))))
 (let ((v_189 (bvnot v_165)))
 (let ((v_190
        (bvor
         (ite (= v_189 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_165 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_191 (bvnot v_190)))
 (let ((v_192 (bvand ((_ int2bv 1) 1) v_191)))
 (let ((v_193 (bvand v_188 v_192)))
 (let ((v_194 (bvand ((_ int2bv 1) 1) v_190)))
 (let ((v_195 (bvor v_193 v_194)))
 (let ((v_196
        (bvor
         (ite (= v_193 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_194 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((act_176 (bvand v_175 v_257)))
 (let ((v_177 (bvadd (vreg_172 prev) ((_ int2bv 3) 1))))
 (let ((v_178 (bvnot act_176)))
 (let ((v_180
        (bvor
         (ite (= v_178 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite
          (= act_176 #b1)
          (vinpt_branch_target_179 inpts)
          ((_ int2bv 3) 0)))))
 (let ((v_181 (ite (= act_176 ((_ int2bv 1) 0)) v_177 v_180)))
 (let ((v_182 (bvor (ite (= v_257 #b1) v_181 ((_ int2bv 3) 0)))))
 (let ((v_171 (bvor (ite (= v_247 #b1) (vreg_170 prev) ((_ int2bv 3) 0)))))
 (let ((v_169 (bvor (ite (= v_144 #b1) v_200 ((_ int2bv 3) 0)))))
 (let ((v_37 (bvnot v_110)))
 (let ((v_38 (bvand v_37 ((_ int2bv 1) 1))))
 (let ((v_39 (bvnot v_240)))
 (let ((v_40 (bvand (vreg_5 prev) v_39)))
 (let ((v_41 (bvor (ite (= v_38 #b1) v_40 ((_ int2bv 1) 0)))))
 (let ((v_43 ((_ extract 8 6) v_27)))
 (let ((v_44 (concat v_30 v_31)))
 (let ((v_45 ((_ extract 2 0) v_28)))
 (let ((v_46 ((_ extract 2 2) v_45)))
 (let ((v_47 ((_ extract 1 0) v_45)))
 (let ((v_48 (concat v_46 v_47)))
 (let ((v_49 (concat v_44 v_48)))
 (let ((v_50 (concat v_43 v_49)))
 (let ((v_51 ((_ extract 4 2) v_236)))
 (let ((v_52 ((_ extract 2 2) v_51)))
 (let ((v_53 ((_ extract 1 0) v_51)))
 (let ((v_54 (concat v_52 v_53)))
 (let ((v_55 ((_ extract 0 0) v_237)))
 (let ((v_56 (concat v_238 v_55)))
 (let ((v_57 (concat v_54 v_56)))
 (let ((v_58 (concat v_50 v_57)))
 (let ((v_59 (bvor (ite (= v_38 #b1) v_58 ((_ int2bv 14) 0)))))
 (let ((v_68 (bvnot ((_ int2bv 1) 0))))
 (let ((v_69 (bvor (ite (= v_68 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_70 (bvand ((_ int2bv 1) 1) v_69)))
 (let ((v_76 ((_ extract 0 0) (vreg_75 prev))))
 (let ((v_88 (bvand v_238 v_87)))
 (let ((v_89 (bvand (vreg_5 prev) v_88)))
 (let ((v_90 (bvand v_89 v_38)))
 (let ((act_91 (bvand v_76 v_90)))
 (let ((v_92 (bvnot v_69)))
 (let ((v_93 (bvand ((_ int2bv 1) 1) v_92)))
 (let ((v_94 (bvand act_91 v_93)))
 (let ((v_95 (bvnot act_91)))
 (let ((v_96 (bvnot (vreg_103 prev))))
 (let ((v_97 (bvor v_84 v_96)))
 (let ((v_98 (bvand v_95 v_97)))
 (let ((v_99 (bvand v_98 v_93)))
 (let ((v_100 (bvor v_94 v_99)))
 (let ((v_101 (bvor v_70 v_100)))
 (let ((v_71 (bvand v_17 v_257)))
 (let ((v_73 (concat (vinpt_mreq_val_72 inpts) v_20)))
 (let ((v_74 (bvor (ite (= v_71 #b1) v_73 ((_ int2bv 4) 0)))))
 (let ((v_102
        (bvor
         (ite (= v_94 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_99 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_70 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_2 (bvnot v_243)))
 (let ((v_3 (bvand v_2 ((_ int2bv 1) 1))))
 (let ((v_219 ((_ extract 8 6) v_218)))
 (let ((v_220 (concat v_20 v_21)))
 (let ((v_221 ((_ extract 2 0) v_18)))
 (let ((v_222 ((_ extract 2 2) v_221)))
 (let ((v_223 ((_ extract 1 0) v_221)))
 (let ((v_224 (concat v_222 v_223)))
 (let ((v_225 (concat v_220 v_224)))
 (let ((v_226 (concat v_219 v_225)))
 (let ((v_227 ((_ extract 4 2) v_15)))
 (let ((v_228 ((_ extract 2 2) v_227)))
 (let ((v_229 ((_ extract 1 0) v_227)))
 (let ((v_230 (concat v_228 v_229)))
 (let ((v_231 (concat v_17 v_173)))
 (let ((v_232 (concat v_230 v_231)))
 (let ((v_233 (concat v_226 v_232)))
 (let ((v_234 (bvor (ite (= v_3 #b1) v_233 ((_ int2bv 14) 0)))))
 (let ((v_4 (bvor (ite (= v_3 #b1) v_256 ((_ int2bv 1) 0)))))
 (let ((v_248 (bvnot v_198)))
 (let ((v_249 (bvand v_139 v_248)))
 (let ((v_250 (bvand (vreg_146 prev) v_249)))
 (let ((v_251 (bvor (ite (= v_247 #b1) v_250 ((_ int2bv 1) 0)))))
 (mkTuple2
  (=> (= v_257 #b1) (= v_264 #b1))
  (mkState_assert_v_264
   (ite (= v_3 #b1) v_4 (vreg_5 prev))
   (ite (= v_38 #b1) v_41 (vreg_42 prev))
   (ite (= v_38 #b1) v_59 (vreg_60 prev))
   (ite (= v_71 #b1) v_74 (vreg_75 prev))
   (ite (= v_101 #b1) v_102 (vreg_103 prev))
   (ite (= v_144 #b1) v_145 (vreg_146 prev))
   (ite (= v_158 #b1) v_159 (vreg_160 prev))
   (ite (= v_144 #b1) v_169 (vreg_170 prev))
   (ite (= v_247 #b1) v_171 (vreg_172 prev))
   (ite (= v_257 #b1) v_182 (vreg_183 prev))
   (ite (= v_195 #b1) v_196 (vreg_197 prev))
   (ite (= v_167 #b1) v_202 (vreg_203 prev))
   (ite (= v_247 #b1) v_216 (vreg_217 prev))
   (ite (= v_3 #b1) v_234 (vreg_235 prev))
   (ite (= v_247 #b1) v_251 (vreg_252 prev))
   (ite
    (= v_257 #b1)
    v_262
    (vreg_263
     prev))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;-------------------------------------------------------------------------------
; Defining the specific chaining of the transition function
(define-fun-rec
 chain_tFun_assert_v_264
 ((xs (ListX Input_assert_v_264)) (prev State_assert_v_264))
 (Tuple2 (ListX Bool) (ListX State_assert_v_264))
 (match
  xs
  ((nil
    (mkTuple2
     (as nil (ListX Bool))
     (cons prev (as nil (ListX State_assert_v_264)))))
   ((cons h t)
    (match
     (tFun_assert_v_264 h prev)
     (((mkTuple2 ret next)
       (match
        (chain_tFun_assert_v_264 t next)
        (((mkTuple2 rets ys) (mkTuple2 (cons ret rets) (cons prev ys))))))))))))
(push)
;-------------------------------------------------------------------------------
(echo "PC correct")
(echo "--------------------------------------------------------------------------------")
(echo "Using bounded verification of depth 7")
(echo "Bounded property refutation:")
(declare-const in0 Input_assert_v_264)
(declare-const in1 Input_assert_v_264)
(declare-const in2 Input_assert_v_264)
(declare-const in3 Input_assert_v_264)
(declare-const in4 Input_assert_v_264)
(declare-const in5 Input_assert_v_264)
(declare-const in6 Input_assert_v_264)
(assert
 (let ((s0
        (mkState_assert_v_264
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 4) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)))
       (inpts
        (cons
         in0
         (cons
          in1
          (cons
           in2
           (cons
            in3
            (cons
             in4
             (cons in5 (cons in6 (as nil (ListX Input_assert_v_264)))))))))))
      (match
       (match
        (tFun_assert_v_264 in0 s0)
        (((mkTuple2 ret0 s1)
          (match
           (tFun_assert_v_264 in1 s1)
           (((mkTuple2 ret1 s2)
             (match
              (tFun_assert_v_264 in2 s2)
              (((mkTuple2 ret2 s3)
                (match
                 (tFun_assert_v_264 in3 s3)
                 (((mkTuple2 ret3 s4)
                   (match
                    (tFun_assert_v_264 in4 s4)
                    (((mkTuple2 ret4 s5)
                      (match
                       (tFun_assert_v_264 in5 s5)
                       (((mkTuple2 ret5 s6)
                         (match
                          (tFun_assert_v_264 in6 s6)
                          (((mkTuple2 ret6 s7)
                            (mkTuple2
                             (cons
                              ret0
                              (cons
                               ret1
                               (cons
                                ret2
                                (cons
                                 ret3
                                 (cons
                                  ret4
                                  (cons
                                   ret5
                                   (cons ret6 (as nil (ListX Bool)))))))))
                             (cons
                              s0
                              (cons
                               s1
                               (cons
                                s2
                                (cons
                                 s3
                                 (cons
                                  s4
                                  (cons
                                   s5
                                   (cons
                                    s6
                                    (as
                                     nil
                                     (ListX State_assert_v_264)))))))))))))))))))))))))))))))
       (((mkTuple2 oks ss) (not (andReduce oks)))))))
(check-sat)
(pop)
(echo "")
;-------------------------------------------------------------------------------
; code gen for "assert_v_357"
;-------------------------------------------------------------------------------
(push)
;-------------------------------------------------------------------------------
; Defining the specific Inputs record type
(declare-datatype
 Input_assert_v_357
 ((mkInput_assert_v_357
   (vinpt_imem_peek_mask_11 (_ BitVec 1))
   (vinpt_rs1_valid_14 (_ BitVec 1))
   (vinpt_rs1_22 (_ BitVec 2))
   (vinpt_mreq_val_72 (_ BitVec 3))
   (vinpt_dmem_put_mask_86 (_ BitVec 1))
   (vinpt_dmem_peek_mask_104 (_ BitVec 1))
   (vinpt_rs2_valid_119 (_ BitVec 1))
   (vinpt_rs2_120 (_ BitVec 2))
   (vinpt_imem_put_mask_163 (_ BitVec 1))
   (vinpt_branch_valid_174 (_ BitVec 1))
   (vinpt_branch_target_179 (_ BitVec 3))
   (vinpt_predicted_pc_199 (_ BitVec 3))
   (vinpt_rd_valid_204 (_ BitVec 1))
   (vinpt_rd_205 (_ BitVec 2))
   (vinpt_is_mem_access_211 (_ BitVec 1))
   (vinpt_can_branch_212 (_ BitVec 1))
   (vinpt_result_270 (_ BitVec 3)))))
;-------------------------------------------------------------------------------
; Defining the specific State record type
(declare-datatype
 State_assert_v_357
 ((mkState_assert_v_357
   (vreg_5 (_ BitVec 1))
   (vreg_42 (_ BitVec 1))
   (vreg_60 (_ BitVec 14))
   (vreg_75 (_ BitVec 4))
   (vreg_103 (_ BitVec 1))
   (vreg_146 (_ BitVec 1))
   (vreg_160 (_ BitVec 1))
   (vreg_170 (_ BitVec 3))
   (vreg_172 (_ BitVec 3))
   (vreg_183 (_ BitVec 3))
   (vreg_197 (_ BitVec 1))
   (vreg_203 (_ BitVec 3))
   (vreg_217 (_ BitVec 14))
   (vreg_235 (_ BitVec 14))
   (vreg_252 (_ BitVec 1))
   (vreg_273 (_ BitVec 3))
   (vreg_277 (_ BitVec 3))
   (vreg_281 (_ BitVec 3))
   (vreg_285 (_ BitVec 3))
   (vreg_294 (_ BitVec 2))
   (vreg_296 (_ BitVec 2))
   (vreg_303 (_ BitVec 3))
   (vreg_305 (_ BitVec 3))
   (vreg_310 (_ BitVec 3))
   (vreg_313 (_ BitVec 3))
   (vreg_317 (_ BitVec 3))
   (vreg_321 (_ BitVec 3))
   (vreg_325 (_ BitVec 3))
   (vreg_328 (_ BitVec 3))
   (vreg_330 (_ BitVec 3))
   (vreg_345 (_ BitVec 2))
   (vreg_351 (_ BitVec 3)))))
;-------------------------------------------------------------------------------
; Defining the specific transition function
(define-fun
 tFun_assert_v_357
 ((inpts Input_assert_v_357) (prev State_assert_v_357))
 (Tuple2 Bool State_assert_v_357)
 (let ((v_1 (bvnot ((_ int2bv 1) 1))))
 (let ((v_236 ((_ extract 4 0) (vreg_235 prev))))
 (let ((v_237 ((_ extract 1 0) v_236)))
 (let ((v_238 ((_ extract 1 1) v_237)))
 (let ((v_77 (bvnot (vreg_103 prev))))
 (let ((v_61 ((_ extract 4 0) (vreg_60 prev))))
 (let ((v_62 ((_ extract 1 0) v_61)))
 (let ((v_63 ((_ extract 1 1) v_62)))
 (let ((v_64 ((_ extract 13 5) (vreg_60 prev))))
 (let ((v_65 ((_ extract 5 0) v_64)))
 (let ((v_66 ((_ extract 5 3) v_65)))
 (let ((v_67 ((_ extract 2 2) v_66)))
 (let ((v_105 (bvand (vreg_103 prev) (vinpt_dmem_peek_mask_104 inpts))))
 (let ((v_106 (bvnot v_105)))
 (let ((v_107 (bvand v_67 v_106)))
 (let ((v_108 (bvand v_63 v_107)))
 (let ((v_78 (bvnot v_108)))
 (let ((v_79 (bvand v_67 v_78)))
 (let ((v_80 (bvand (vreg_42 prev) v_79)))
 (let ((v_81 (bvand v_80 ((_ int2bv 1) 1))))
 (let ((v_82 (bvand v_63 v_81)))
 (let ((v_83 (bvnot v_82)))
 (let ((v_84
        (bvor
         (ite (= v_83 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_82 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_85 (bvor v_77 v_84)))
 (let ((v_87 (bvand v_85 (vinpt_dmem_put_mask_86 inpts))))
 (let ((v_239 (bvnot v_87)))
 (let ((v_240 (bvand v_238 v_239)))
 (let ((v_36 (bvnot ((_ int2bv 1) 1))))
 (let ((v_109 (bvand (vreg_42 prev) v_108)))
 (let ((v_110
        (bvor
         (ite (= v_36 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_109 ((_ int2bv 1) 0)))))
 (let ((v_241 (bvor v_240 v_110)))
 (let ((v_242 (bvand (vreg_5 prev) v_241)))
 (let ((v_243
        (bvor
         (ite (= v_1 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_242 ((_ int2bv 1) 0)))))
 (let ((v_253 (bvnot v_243)))
 (let ((v_184 (bvnot (ite (= (vreg_172 prev) (vreg_183 prev)) #b1 #b0))))
 (let ((v_254 (bvnot v_184)))
 (let ((v_255 (bvand v_253 v_254)))
 (let ((v_256 (bvand (vreg_252 prev) v_255)))
 (let ((v_257 (bvand v_256 ((_ int2bv 1) 1))))
 (let ((v_218 ((_ extract 13 5) (vreg_217 prev))))
 (let ((v_18 ((_ extract 5 0) v_218)))
 (let ((v_221 ((_ extract 2 0) v_18)))
 (let ((v_222 ((_ extract 2 2) v_221)))
 (let ((v_266 (bvnot v_222)))
 (let ((v_223 ((_ extract 1 0) v_221)))
 (let ((v_286
        (ite
         (= v_223 ((_ int2bv 2) 0))
         (vreg_273 prev)
         (ite
          (= v_223 ((_ int2bv 2) 1))
          (vreg_277 prev)
          (ite (= v_223 ((_ int2bv 2) 2)) (vreg_281 prev) (vreg_285 prev))))))
 (let ((v_27 ((_ extract 13 5) (vreg_235 prev))))
 (let ((v_28 ((_ extract 5 0) v_27)))
 (let ((v_29 ((_ extract 5 3) v_28)))
 (let ((v_30 ((_ extract 2 2) v_29)))
 (let ((v_31 ((_ extract 1 0) v_29)))
 (let ((v_287 (ite (= v_31 v_223) #b1 #b0)))
 (let ((v_288 (bvand v_30 v_287)))
 (let ((v_289 (bvand (vreg_5 prev) v_288)))
 (let ((v_111 ((_ extract 1 0) v_66)))
 (let ((v_290 (ite (= v_111 v_223) #b1 #b0)))
 (let ((v_291 (bvand v_67 v_290)))
 (let ((v_292 (bvand (vreg_42 prev) v_291)))
 (let ((v_297 (ite (= (vreg_294 prev) (vreg_296 prev)) #b1 #b0)))
 (let ((v_331
        (ite (= v_297 ((_ int2bv 1) 0)) (vreg_328 prev) (vreg_330 prev))))
 (let ((v_332 (ite (= v_292 ((_ int2bv 1) 0)) v_331 (vreg_305 prev))))
 (let ((v_333 (ite (= v_289 ((_ int2bv 1) 0)) v_332 (vreg_303 prev))))
 (let ((v_334 (ite (= v_286 v_333) #b1 #b0)))
 (let ((v_335 (bvor v_266 v_334)))
 (let ((v_15 ((_ extract 4 0) (vreg_217 prev))))
 (let ((v_227 ((_ extract 4 2) v_15)))
 (let ((v_228 ((_ extract 2 2) v_227)))
 (let ((v_336 (bvnot v_228)))
 (let ((v_229 ((_ extract 1 0) v_227)))
 (let ((v_337
        (ite
         (= v_229 ((_ int2bv 2) 0))
         (vreg_273 prev)
         (ite
          (= v_229 ((_ int2bv 2) 1))
          (vreg_277 prev)
          (ite (= v_229 ((_ int2bv 2) 2)) (vreg_281 prev) (vreg_285 prev))))))
 (let ((v_338 (ite (= v_31 v_229) #b1 #b0)))
 (let ((v_339 (bvand v_30 v_338)))
 (let ((v_340 (bvand (vreg_5 prev) v_339)))
 (let ((v_341 (ite (= v_111 v_229) #b1 #b0)))
 (let ((v_342 (bvand v_67 v_341)))
 (let ((v_343 (bvand (vreg_42 prev) v_342)))
 (let ((v_346 (ite (= (vreg_294 prev) (vreg_345 prev)) #b1 #b0)))
 (let ((v_352
        (ite (= v_346 ((_ int2bv 1) 0)) (vreg_351 prev) (vreg_330 prev))))
 (let ((v_353 (ite (= v_343 ((_ int2bv 1) 0)) v_352 (vreg_305 prev))))
 (let ((v_354 (ite (= v_340 ((_ int2bv 1) 0)) v_353 (vreg_303 prev))))
 (let ((v_355 (ite (= v_337 v_354) #b1 #b0)))
 (let ((v_356 (bvor v_336 v_355)))
 (let ((v_357 (bvand v_335 v_356)))
 (let ((v_0 (bvnot ((_ int2bv 1) 1))))
 (let ((v_244 (bvand (vreg_252 prev) v_243)))
 (let ((v_245
        (bvor
         (ite (= v_0 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_244 ((_ int2bv 1) 0)))))
 (let ((v_246 (bvnot v_245)))
 (let ((v_247 (bvand v_246 ((_ int2bv 1) 1))))
 (let ((v_347 (bvnot v_247)))
 (let ((v_348
        (bvor
         (ite (= v_347 #b1) (vreg_345 prev) ((_ int2bv 2) 0))
         (ite (= v_247 #b1) (vinpt_rs2_120 inpts) ((_ int2bv 2) 0)))))
 (let ((v_349
        (ite
         (= v_348 ((_ int2bv 2) 0))
         (vreg_313 prev)
         (ite
          (= v_348 ((_ int2bv 2) 1))
          (vreg_317 prev)
          (ite (= v_348 ((_ int2bv 2) 2)) (vreg_321 prev) (vreg_325 prev))))))
 (let ((v_350 (bvor (ite (= ((_ int2bv 1) 1) #b1) v_349 ((_ int2bv 3) 0)))))
 (let ((v_322 (ite (= v_111 ((_ int2bv 2) 3)) #b1 #b0)))
 (let ((v_323 (bvand v_322 v_81)))
 (let ((v_311
        (ite (= v_63 ((_ int2bv 1) 0)) (vreg_305 prev) (vreg_310 prev))))
 (let ((v_324 (bvor (ite (= v_323 #b1) v_311 ((_ int2bv 3) 0)))))
 (let ((v_76 ((_ extract 0 0) (vreg_75 prev))))
 (let ((v_88 (bvand v_238 v_87)))
 (let ((v_89 (bvand (vreg_5 prev) v_88)))
 (let ((v_37 (bvnot v_110)))
 (let ((v_38 (bvand v_37 ((_ int2bv 1) 1))))
 (let ((v_90 (bvand v_89 v_38)))
 (let ((act_91 (bvand v_76 v_90)))
 (let ((v_68 (bvnot ((_ int2bv 1) 0))))
 (let ((v_69 (bvor (ite (= v_68 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_92 (bvnot v_69)))
 (let ((v_93 (bvand ((_ int2bv 1) 1) v_92)))
 (let ((v_94 (bvand act_91 v_93)))
 (let ((v_16 ((_ extract 1 0) v_15)))
 (let ((v_17 ((_ extract 1 1) v_16)))
 (let ((v_71 (bvand v_17 v_257)))
 (let ((v_19 ((_ extract 5 3) v_18)))
 (let ((v_20 ((_ extract 2 2) v_19)))
 (let ((v_73 (concat (vinpt_mreq_val_72 inpts) v_20)))
 (let ((v_74 (bvor (ite (= v_71 #b1) v_73 ((_ int2bv 4) 0)))))
 (let ((v_306 (bvnot act_91)))
 (let ((v_307 ((_ extract 3 1) (vreg_75 prev))))
 (let ((v_308
        (bvor
         (ite (= v_306 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite (= act_91 #b1) v_307 ((_ int2bv 3) 0)))))
 (let ((v_309 (bvor (ite (= v_94 #b1) v_308 ((_ int2bv 3) 0)))))
 (let ((v_318 (ite (= v_111 ((_ int2bv 2) 2)) #b1 #b0)))
 (let ((v_319 (bvand v_318 v_81)))
 (let ((v_320 (bvor (ite (= v_319 #b1) v_311 ((_ int2bv 3) 0)))))
 (let ((v_314 (ite (= v_111 ((_ int2bv 2) 1)) #b1 #b0)))
 (let ((v_315 (bvand v_314 v_81)))
 (let ((v_316 (bvor (ite (= v_315 #b1) v_311 ((_ int2bv 3) 0)))))
 (let ((v_300 (ite (= v_111 ((_ int2bv 2) 0)) #b1 #b0)))
 (let ((v_301 (bvand v_300 v_81)))
 (let ((v_312 (bvor (ite (= v_301 #b1) v_311 ((_ int2bv 3) 0)))))
 (let ((v_344
        (bvor (ite (= v_247 #b1) (vinpt_rs2_120 inpts) ((_ int2bv 2) 0)))))
 (let ((v_268 (bvand v_20 v_257)))
 (let ((v_302
        (bvor (ite (= v_268 #b1) (vinpt_result_270 inpts) ((_ int2bv 3) 0)))))
 (let ((v_304 (bvor (ite (= v_38 #b1) (vreg_303 prev) ((_ int2bv 3) 0)))))
 (let ((v_329 (bvor (ite (= v_81 #b1) v_311 ((_ int2bv 3) 0)))))
 (let ((v_298 (bvnot v_247)))
 (let ((v_299
        (bvor
         (ite (= v_298 #b1) (vreg_296 prev) ((_ int2bv 2) 0))
         (ite (= v_247 #b1) (vinpt_rs1_22 inpts) ((_ int2bv 2) 0)))))
 (let ((v_326
        (ite
         (= v_299 ((_ int2bv 2) 0))
         (vreg_313 prev)
         (ite
          (= v_299 ((_ int2bv 2) 1))
          (vreg_317 prev)
          (ite (= v_299 ((_ int2bv 2) 2)) (vreg_321 prev) (vreg_325 prev))))))
 (let ((v_327 (bvor (ite (= ((_ int2bv 1) 1) #b1) v_326 ((_ int2bv 3) 0)))))
 (let ((v_295
        (bvor (ite (= v_247 #b1) (vinpt_rs1_22 inpts) ((_ int2bv 2) 0)))))
 (let ((v_293 (bvor (ite (= v_81 #b1) v_111 ((_ int2bv 2) 0)))))
 (let ((v_21 ((_ extract 1 0) v_19)))
 (let ((v_282 (ite (= v_21 ((_ int2bv 2) 3)) #b1 #b0)))
 (let ((v_283 (bvand v_282 v_268)))
 (let ((v_271
        (ite
         (= v_17 ((_ int2bv 1) 0))
         (vinpt_result_270 inpts)
         (vinpt_mreq_val_72 inpts))))
 (let ((v_284 (bvor (ite (= v_283 #b1) v_271 ((_ int2bv 3) 0)))))
 (let ((v_278 (ite (= v_21 ((_ int2bv 2) 2)) #b1 #b0)))
 (let ((v_279 (bvand v_278 v_268)))
 (let ((v_280 (bvor (ite (= v_279 #b1) v_271 ((_ int2bv 3) 0)))))
 (let ((v_274 (ite (= v_21 ((_ int2bv 2) 1)) #b1 #b0)))
 (let ((v_275 (bvand v_274 v_268)))
 (let ((v_276 (bvor (ite (= v_275 #b1) v_271 ((_ int2bv 3) 0)))))
 (let ((v_267 (ite (= v_21 ((_ int2bv 2) 0)) #b1 #b0)))
 (let ((v_269 (bvand v_267 v_268)))
 (let ((v_272 (bvor (ite (= v_269 #b1) v_271 ((_ int2bv 3) 0)))))
 (let ((v_206 (concat (vinpt_rd_valid_204 inpts) (vinpt_rd_205 inpts))))
 (let ((v_207 (concat (vinpt_rs1_valid_14 inpts) (vinpt_rs1_22 inpts))))
 (let ((v_208 (concat v_206 v_207)))
 (let ((v_209 (concat (vreg_203 prev) v_208)))
 (let ((v_210 (concat (vinpt_rs2_valid_119 inpts) (vinpt_rs2_120 inpts))))
 (let ((v_213
        (concat (vinpt_is_mem_access_211 inpts) (vinpt_can_branch_212 inpts))))
 (let ((v_214 (concat v_210 v_213)))
 (let ((v_215 (concat v_209 v_214)))
 (let ((v_216 (bvor (ite (= v_247 #b1) v_215 ((_ int2bv 14) 0)))))
 (let ((v_161 (bvnot (vreg_160 prev))))
 (let ((v_12 (bvand (vreg_160 prev) (vinpt_imem_peek_mask_11 inpts))))
 (let ((v_13 (bvnot v_245)))
 (let ((v_23 (ite (= v_21 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_24 (bvand v_20 v_23)))
 (let ((v_25 (bvand v_17 v_24)))
 (let ((v_26 (bvand (vreg_252 prev) v_25)))
 (let ((v_32 (ite (= v_31 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_33 (bvand v_30 v_32)))
 (let ((v_34 (bvand v_238 v_33)))
 (let ((v_35 (bvand (vreg_5 prev) v_34)))
 (let ((v_112 (ite (= v_111 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_113 (bvand v_67 v_112)))
 (let ((v_114 (bvand v_63 v_113)))
 (let ((v_115 (bvand v_110 v_114)))
 (let ((v_116 (bvor v_35 v_115)))
 (let ((v_117 (bvor v_26 v_116)))
 (let ((v_118 (bvand (vinpt_rs1_valid_14 inpts) v_117)))
 (let ((v_121 (ite (= v_21 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_122 (bvand v_20 v_121)))
 (let ((v_123 (bvand v_17 v_122)))
 (let ((v_124 (bvand (vreg_252 prev) v_123)))
 (let ((v_125 (ite (= v_31 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_126 (bvand v_30 v_125)))
 (let ((v_127 (bvand v_238 v_126)))
 (let ((v_128 (bvand (vreg_5 prev) v_127)))
 (let ((v_129 (ite (= v_111 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_130 (bvand v_67 v_129)))
 (let ((v_131 (bvand v_63 v_130)))
 (let ((v_132 (bvand v_110 v_131)))
 (let ((v_133 (bvor v_128 v_132)))
 (let ((v_134 (bvor v_124 v_133)))
 (let ((v_135 (bvand (vinpt_rs2_valid_119 inpts) v_134)))
 (let ((v_136 (bvor v_118 v_135)))
 (let ((v_137 (bvnot v_136)))
 (let ((v_138 (bvand v_13 v_137)))
 (let ((v_139 (bvand v_12 v_138)))
 (let ((v_147 (bvand (vreg_146 prev) v_139)))
 (let ((v_148 (bvand v_147 ((_ int2bv 1) 1))))
 (let ((v_149 (bvnot v_148)))
 (let ((v_150
        (bvor
         (ite (= v_149 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_148 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_162 (bvor v_161 v_150)))
 (let ((v_164 (bvand v_162 (vinpt_imem_put_mask_163 inpts))))
 (let ((v_10 (bvnot ((_ int2bv 1) 1))))
 (let ((v_140 (bvnot v_139)))
 (let ((v_141 (bvand (vreg_146 prev) v_140)))
 (let ((v_142
        (bvor
         (ite (= v_10 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_141 ((_ int2bv 1) 0)))))
 (let ((v_143 (bvnot v_142)))
 (let ((v_144 (bvand v_143 ((_ int2bv 1) 1))))
 (let ((v_165 (bvand v_164 v_144)))
 (let ((act_166 (bvand ((_ int2bv 1) 1) v_165)))
 (let ((v_6 (bvnot ((_ int2bv 1) 0))))
 (let ((v_7 (bvor (ite (= v_6 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_154 (bvnot v_7)))
 (let ((v_155 (bvand ((_ int2bv 1) 1) v_154)))
 (let ((v_167 (bvand act_166 v_155)))
 (let ((v_145 (bvor (ite (= v_144 #b1) v_164 ((_ int2bv 1) 0)))))
 (let ((v_8 (bvand ((_ int2bv 1) 1) v_7)))
 (let ((v_9 (bvnot act_166)))
 (let ((v_151 (bvnot (vreg_160 prev))))
 (let ((v_152 (bvor v_150 v_151)))
 (let ((v_153 (bvand v_9 v_152)))
 (let ((v_156 (bvand v_153 v_155)))
 (let ((v_157 (bvor v_167 v_156)))
 (let ((v_158 (bvor v_8 v_157)))
 (let ((v_159
        (bvor
         (ite (= v_167 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_156 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_8 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_168 (bvnot act_166)))
 (let ((v_185 (bvand (vreg_252 prev) v_184)))
 (let ((v_186 (bvand v_185 ((_ int2bv 1) 1))))
 (let ((v_187 (bvnot v_186)))
 (let ((v_188
        (bvor
         (ite (= v_187 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_186 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_198 (bvor v_188 (vreg_197 prev))))
 (let ((v_200
        (ite
         (= v_198 ((_ int2bv 1) 0))
         (vinpt_predicted_pc_199 inpts)
         (vreg_183 prev))))
 (let ((v_201
        (bvor
         (ite (= v_168 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite (= act_166 #b1) v_200 ((_ int2bv 3) 0)))))
 (let ((v_202 (bvor (ite (= v_167 #b1) v_201 ((_ int2bv 3) 0)))))
 (let ((v_189 (bvnot v_165)))
 (let ((v_190
        (bvor
         (ite (= v_189 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_165 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_191 (bvnot v_190)))
 (let ((v_192 (bvand ((_ int2bv 1) 1) v_191)))
 (let ((v_193 (bvand v_188 v_192)))
 (let ((v_194 (bvand ((_ int2bv 1) 1) v_190)))
 (let ((v_195 (bvor v_193 v_194)))
 (let ((v_196
        (bvor
         (ite (= v_193 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_194 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_173 ((_ extract 0 0) v_16)))
 (let ((v_175 (bvand v_173 (vinpt_branch_valid_174 inpts))))
 (let ((act_176 (bvand v_175 v_257)))
 (let ((v_177 (bvadd (vreg_172 prev) ((_ int2bv 3) 1))))
 (let ((v_178 (bvnot act_176)))
 (let ((v_180
        (bvor
         (ite (= v_178 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite
          (= act_176 #b1)
          (vinpt_branch_target_179 inpts)
          ((_ int2bv 3) 0)))))
 (let ((v_181 (ite (= act_176 ((_ int2bv 1) 0)) v_177 v_180)))
 (let ((v_182 (bvor (ite (= v_257 #b1) v_181 ((_ int2bv 3) 0)))))
 (let ((v_171 (bvor (ite (= v_247 #b1) (vreg_170 prev) ((_ int2bv 3) 0)))))
 (let ((v_169 (bvor (ite (= v_144 #b1) v_200 ((_ int2bv 3) 0)))))
 (let ((v_39 (bvnot v_240)))
 (let ((v_40 (bvand (vreg_5 prev) v_39)))
 (let ((v_41 (bvor (ite (= v_38 #b1) v_40 ((_ int2bv 1) 0)))))
 (let ((v_43 ((_ extract 8 6) v_27)))
 (let ((v_44 (concat v_30 v_31)))
 (let ((v_45 ((_ extract 2 0) v_28)))
 (let ((v_46 ((_ extract 2 2) v_45)))
 (let ((v_47 ((_ extract 1 0) v_45)))
 (let ((v_48 (concat v_46 v_47)))
 (let ((v_49 (concat v_44 v_48)))
 (let ((v_50 (concat v_43 v_49)))
 (let ((v_51 ((_ extract 4 2) v_236)))
 (let ((v_52 ((_ extract 2 2) v_51)))
 (let ((v_53 ((_ extract 1 0) v_51)))
 (let ((v_54 (concat v_52 v_53)))
 (let ((v_55 ((_ extract 0 0) v_237)))
 (let ((v_56 (concat v_238 v_55)))
 (let ((v_57 (concat v_54 v_56)))
 (let ((v_58 (concat v_50 v_57)))
 (let ((v_59 (bvor (ite (= v_38 #b1) v_58 ((_ int2bv 14) 0)))))
 (let ((v_70 (bvand ((_ int2bv 1) 1) v_69)))
 (let ((v_95 (bvnot act_91)))
 (let ((v_96 (bvnot (vreg_103 prev))))
 (let ((v_97 (bvor v_84 v_96)))
 (let ((v_98 (bvand v_95 v_97)))
 (let ((v_99 (bvand v_98 v_93)))
 (let ((v_100 (bvor v_94 v_99)))
 (let ((v_101 (bvor v_70 v_100)))
 (let ((v_102
        (bvor
         (ite (= v_94 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_99 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_70 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_2 (bvnot v_243)))
 (let ((v_3 (bvand v_2 ((_ int2bv 1) 1))))
 (let ((v_219 ((_ extract 8 6) v_218)))
 (let ((v_220 (concat v_20 v_21)))
 (let ((v_224 (concat v_222 v_223)))
 (let ((v_225 (concat v_220 v_224)))
 (let ((v_226 (concat v_219 v_225)))
 (let ((v_230 (concat v_228 v_229)))
 (let ((v_231 (concat v_17 v_173)))
 (let ((v_232 (concat v_230 v_231)))
 (let ((v_233 (concat v_226 v_232)))
 (let ((v_234 (bvor (ite (= v_3 #b1) v_233 ((_ int2bv 14) 0)))))
 (let ((v_4 (bvor (ite (= v_3 #b1) v_256 ((_ int2bv 1) 0)))))
 (let ((v_248 (bvnot v_198)))
 (let ((v_249 (bvand v_139 v_248)))
 (let ((v_250 (bvand (vreg_146 prev) v_249)))
 (let ((v_251 (bvor (ite (= v_247 #b1) v_250 ((_ int2bv 1) 0)))))
 (mkTuple2
  (=> (= v_257 #b1) (= v_357 #b1))
  (mkState_assert_v_357
   (ite (= v_3 #b1) v_4 (vreg_5 prev))
   (ite (= v_38 #b1) v_41 (vreg_42 prev))
   (ite (= v_38 #b1) v_59 (vreg_60 prev))
   (ite (= v_71 #b1) v_74 (vreg_75 prev))
   (ite (= v_101 #b1) v_102 (vreg_103 prev))
   (ite (= v_144 #b1) v_145 (vreg_146 prev))
   (ite (= v_158 #b1) v_159 (vreg_160 prev))
   (ite (= v_144 #b1) v_169 (vreg_170 prev))
   (ite (= v_247 #b1) v_171 (vreg_172 prev))
   (ite (= v_257 #b1) v_182 (vreg_183 prev))
   (ite (= v_195 #b1) v_196 (vreg_197 prev))
   (ite (= v_167 #b1) v_202 (vreg_203 prev))
   (ite (= v_247 #b1) v_216 (vreg_217 prev))
   (ite (= v_3 #b1) v_234 (vreg_235 prev))
   (ite (= v_247 #b1) v_251 (vreg_252 prev))
   (ite (= v_269 #b1) v_272 (vreg_273 prev))
   (ite (= v_275 #b1) v_276 (vreg_277 prev))
   (ite (= v_279 #b1) v_280 (vreg_281 prev))
   (ite (= v_283 #b1) v_284 (vreg_285 prev))
   (ite (= v_81 #b1) v_293 (vreg_294 prev))
   (ite (= v_247 #b1) v_295 (vreg_296 prev))
   (ite (= v_268 #b1) v_302 (vreg_303 prev))
   (ite (= v_38 #b1) v_304 (vreg_305 prev))
   (ite (= v_94 #b1) v_309 (vreg_310 prev))
   (ite (= v_301 #b1) v_312 (vreg_313 prev))
   (ite (= v_315 #b1) v_316 (vreg_317 prev))
   (ite (= v_319 #b1) v_320 (vreg_321 prev))
   (ite (= v_323 #b1) v_324 (vreg_325 prev))
   (ite (= ((_ int2bv 1) 1) #b1) v_327 (vreg_328 prev))
   (ite (= v_81 #b1) v_329 (vreg_330 prev))
   (ite (= v_247 #b1) v_344 (vreg_345 prev))
   (ite
    (= ((_ int2bv 1) 1) #b1)
    v_350
    (vreg_351
     prev))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;-------------------------------------------------------------------------------
; Defining the specific chaining of the transition function
(define-fun-rec
 chain_tFun_assert_v_357
 ((xs (ListX Input_assert_v_357)) (prev State_assert_v_357))
 (Tuple2 (ListX Bool) (ListX State_assert_v_357))
 (match
  xs
  ((nil
    (mkTuple2
     (as nil (ListX Bool))
     (cons prev (as nil (ListX State_assert_v_357)))))
   ((cons h t)
    (match
     (tFun_assert_v_357 h prev)
     (((mkTuple2 ret next)
       (match
        (chain_tFun_assert_v_357 t next)
        (((mkTuple2 rets ys) (mkTuple2 (cons ret rets) (cons prev ys))))))))))))
(push)
;-------------------------------------------------------------------------------
(echo "Operands correct")
(echo "--------------------------------------------------------------------------------")
(echo "Using bounded verification of depth 7")
(echo "Bounded property refutation:")
(declare-const in0 Input_assert_v_357)
(declare-const in1 Input_assert_v_357)
(declare-const in2 Input_assert_v_357)
(declare-const in3 Input_assert_v_357)
(declare-const in4 Input_assert_v_357)
(declare-const in5 Input_assert_v_357)
(declare-const in6 Input_assert_v_357)
(assert
 (let ((s0
        (mkState_assert_v_357
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 4) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 2) 0)
         ((_ int2bv 2) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 2) 0)
         ((_ int2bv 3) 0)))
       (inpts
        (cons
         in0
         (cons
          in1
          (cons
           in2
           (cons
            in3
            (cons
             in4
             (cons in5 (cons in6 (as nil (ListX Input_assert_v_357)))))))))))
      (match
       (match
        (tFun_assert_v_357 in0 s0)
        (((mkTuple2 ret0 s1)
          (match
           (tFun_assert_v_357 in1 s1)
           (((mkTuple2 ret1 s2)
             (match
              (tFun_assert_v_357 in2 s2)
              (((mkTuple2 ret2 s3)
                (match
                 (tFun_assert_v_357 in3 s3)
                 (((mkTuple2 ret3 s4)
                   (match
                    (tFun_assert_v_357 in4 s4)
                    (((mkTuple2 ret4 s5)
                      (match
                       (tFun_assert_v_357 in5 s5)
                       (((mkTuple2 ret5 s6)
                         (match
                          (tFun_assert_v_357 in6 s6)
                          (((mkTuple2 ret6 s7)
                            (mkTuple2
                             (cons
                              ret0
                              (cons
                               ret1
                               (cons
                                ret2
                                (cons
                                 ret3
                                 (cons
                                  ret4
                                  (cons
                                   ret5
                                   (cons ret6 (as nil (ListX Bool)))))))))
                             (cons
                              s0
                              (cons
                               s1
                               (cons
                                s2
                                (cons
                                 s3
                                 (cons
                                  s4
                                  (cons
                                   s5
                                   (cons
                                    s6
                                    (as
                                     nil
                                     (ListX State_assert_v_357)))))))))))))))))))))))))))))))
       (((mkTuple2 oks ss) (not (andReduce oks)))))))
(check-sat)
(pop)
(echo "")
;-------------------------------------------------------------------------------
; code gen for "assert_v_369"
;-------------------------------------------------------------------------------
(push)
;-------------------------------------------------------------------------------
; Defining the specific Inputs record type
(declare-datatype
 Input_assert_v_369
 ((mkInput_assert_v_369
   (vinpt_imem_peek_mask_11 (_ BitVec 1))
   (vinpt_rs1_valid_14 (_ BitVec 1))
   (vinpt_rs1_22 (_ BitVec 2))
   (vinpt_mreq_val_72 (_ BitVec 3))
   (vinpt_dmem_put_mask_86 (_ BitVec 1))
   (vinpt_dmem_peek_mask_104 (_ BitVec 1))
   (vinpt_rs2_valid_119 (_ BitVec 1))
   (vinpt_rs2_120 (_ BitVec 2))
   (vinpt_imem_put_mask_163 (_ BitVec 1))
   (vinpt_branch_valid_174 (_ BitVec 1))
   (vinpt_branch_target_179 (_ BitVec 3))
   (vinpt_predicted_pc_199 (_ BitVec 3))
   (vinpt_rd_valid_204 (_ BitVec 1))
   (vinpt_rd_205 (_ BitVec 2))
   (vinpt_is_mem_access_211 (_ BitVec 1))
   (vinpt_can_branch_212 (_ BitVec 1)))))
;-------------------------------------------------------------------------------
; Defining the specific State record type
(declare-datatype
 State_assert_v_369
 ((mkState_assert_v_369
   (vreg_5 (_ BitVec 1))
   (vreg_42 (_ BitVec 1))
   (vreg_60 (_ BitVec 14))
   (vreg_75 (_ BitVec 4))
   (vreg_103 (_ BitVec 1))
   (vreg_146 (_ BitVec 1))
   (vreg_160 (_ BitVec 1))
   (vreg_170 (_ BitVec 3))
   (vreg_172 (_ BitVec 3))
   (vreg_183 (_ BitVec 3))
   (vreg_197 (_ BitVec 1))
   (vreg_203 (_ BitVec 3))
   (vreg_217 (_ BitVec 14))
   (vreg_235 (_ BitVec 14))
   (vreg_252 (_ BitVec 1))
   (vreg_368 (_ BitVec 2)))))
;-------------------------------------------------------------------------------
; Defining the specific transition function
(define-fun
 tFun_assert_v_369
 ((inpts Input_assert_v_369) (prev State_assert_v_369))
 (Tuple2 Bool State_assert_v_369)
 (let ((v_369 (ite (bvule (vreg_368 prev) ((_ int2bv 2) 1)) #b1 #b0)))
 (let ((v_0 (bvnot ((_ int2bv 1) 1))))
 (let ((v_1 (bvnot ((_ int2bv 1) 1))))
 (let ((v_236 ((_ extract 4 0) (vreg_235 prev))))
 (let ((v_237 ((_ extract 1 0) v_236)))
 (let ((v_238 ((_ extract 1 1) v_237)))
 (let ((v_77 (bvnot (vreg_103 prev))))
 (let ((v_61 ((_ extract 4 0) (vreg_60 prev))))
 (let ((v_62 ((_ extract 1 0) v_61)))
 (let ((v_63 ((_ extract 1 1) v_62)))
 (let ((v_64 ((_ extract 13 5) (vreg_60 prev))))
 (let ((v_65 ((_ extract 5 0) v_64)))
 (let ((v_66 ((_ extract 5 3) v_65)))
 (let ((v_67 ((_ extract 2 2) v_66)))
 (let ((v_105 (bvand (vreg_103 prev) (vinpt_dmem_peek_mask_104 inpts))))
 (let ((v_106 (bvnot v_105)))
 (let ((v_107 (bvand v_67 v_106)))
 (let ((v_108 (bvand v_63 v_107)))
 (let ((v_78 (bvnot v_108)))
 (let ((v_79 (bvand v_67 v_78)))
 (let ((v_80 (bvand (vreg_42 prev) v_79)))
 (let ((v_81 (bvand v_80 ((_ int2bv 1) 1))))
 (let ((v_82 (bvand v_63 v_81)))
 (let ((v_83 (bvnot v_82)))
 (let ((v_84
        (bvor
         (ite (= v_83 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_82 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_85 (bvor v_77 v_84)))
 (let ((v_87 (bvand v_85 (vinpt_dmem_put_mask_86 inpts))))
 (let ((v_239 (bvnot v_87)))
 (let ((v_240 (bvand v_238 v_239)))
 (let ((v_36 (bvnot ((_ int2bv 1) 1))))
 (let ((v_109 (bvand (vreg_42 prev) v_108)))
 (let ((v_110
        (bvor
         (ite (= v_36 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_109 ((_ int2bv 1) 0)))))
 (let ((v_241 (bvor v_240 v_110)))
 (let ((v_242 (bvand (vreg_5 prev) v_241)))
 (let ((v_243
        (bvor
         (ite (= v_1 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_242 ((_ int2bv 1) 0)))))
 (let ((v_244 (bvand (vreg_252 prev) v_243)))
 (let ((v_245
        (bvor
         (ite (= v_0 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_244 ((_ int2bv 1) 0)))))
 (let ((v_359 (bvnot v_245)))
 (let ((v_360 (bvand (vreg_252 prev) v_359)))
 (let ((v_361 (bvand v_360 ((_ int2bv 1) 1))))
 (let ((v_184 (bvnot (ite (= (vreg_172 prev) (vreg_183 prev)) #b1 #b0))))
 (let ((v_185 (bvand (vreg_252 prev) v_184)))
 (let ((v_186 (bvand v_185 ((_ int2bv 1) 1))))
 (let ((v_187 (bvnot v_186)))
 (let ((v_188
        (bvor
         (ite (= v_187 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_186 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_198 (bvor v_188 (vreg_197 prev))))
 (let ((v_362 (bvnot v_198)))
 (let ((v_363 (bvand v_361 v_362)))
 (let ((v_364 (bvand v_361 v_198)))
 (let ((v_365 (bvor v_363 v_364)))
 (let ((v_161 (bvnot (vreg_160 prev))))
 (let ((v_12 (bvand (vreg_160 prev) (vinpt_imem_peek_mask_11 inpts))))
 (let ((v_13 (bvnot v_245)))
 (let ((v_15 ((_ extract 4 0) (vreg_217 prev))))
 (let ((v_16 ((_ extract 1 0) v_15)))
 (let ((v_17 ((_ extract 1 1) v_16)))
 (let ((v_218 ((_ extract 13 5) (vreg_217 prev))))
 (let ((v_18 ((_ extract 5 0) v_218)))
 (let ((v_19 ((_ extract 5 3) v_18)))
 (let ((v_20 ((_ extract 2 2) v_19)))
 (let ((v_21 ((_ extract 1 0) v_19)))
 (let ((v_23 (ite (= v_21 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_24 (bvand v_20 v_23)))
 (let ((v_25 (bvand v_17 v_24)))
 (let ((v_26 (bvand (vreg_252 prev) v_25)))
 (let ((v_27 ((_ extract 13 5) (vreg_235 prev))))
 (let ((v_28 ((_ extract 5 0) v_27)))
 (let ((v_29 ((_ extract 5 3) v_28)))
 (let ((v_30 ((_ extract 2 2) v_29)))
 (let ((v_31 ((_ extract 1 0) v_29)))
 (let ((v_32 (ite (= v_31 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_33 (bvand v_30 v_32)))
 (let ((v_34 (bvand v_238 v_33)))
 (let ((v_35 (bvand (vreg_5 prev) v_34)))
 (let ((v_111 ((_ extract 1 0) v_66)))
 (let ((v_112 (ite (= v_111 (vinpt_rs1_22 inpts)) #b1 #b0)))
 (let ((v_113 (bvand v_67 v_112)))
 (let ((v_114 (bvand v_63 v_113)))
 (let ((v_115 (bvand v_110 v_114)))
 (let ((v_116 (bvor v_35 v_115)))
 (let ((v_117 (bvor v_26 v_116)))
 (let ((v_118 (bvand (vinpt_rs1_valid_14 inpts) v_117)))
 (let ((v_121 (ite (= v_21 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_122 (bvand v_20 v_121)))
 (let ((v_123 (bvand v_17 v_122)))
 (let ((v_124 (bvand (vreg_252 prev) v_123)))
 (let ((v_125 (ite (= v_31 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_126 (bvand v_30 v_125)))
 (let ((v_127 (bvand v_238 v_126)))
 (let ((v_128 (bvand (vreg_5 prev) v_127)))
 (let ((v_129 (ite (= v_111 (vinpt_rs2_120 inpts)) #b1 #b0)))
 (let ((v_130 (bvand v_67 v_129)))
 (let ((v_131 (bvand v_63 v_130)))
 (let ((v_132 (bvand v_110 v_131)))
 (let ((v_133 (bvor v_128 v_132)))
 (let ((v_134 (bvor v_124 v_133)))
 (let ((v_135 (bvand (vinpt_rs2_valid_119 inpts) v_134)))
 (let ((v_136 (bvor v_118 v_135)))
 (let ((v_137 (bvnot v_136)))
 (let ((v_138 (bvand v_13 v_137)))
 (let ((v_139 (bvand v_12 v_138)))
 (let ((v_147 (bvand (vreg_146 prev) v_139)))
 (let ((v_148 (bvand v_147 ((_ int2bv 1) 1))))
 (let ((v_149 (bvnot v_148)))
 (let ((v_150
        (bvor
         (ite (= v_149 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_148 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_162 (bvor v_161 v_150)))
 (let ((v_164 (bvand v_162 (vinpt_imem_put_mask_163 inpts))))
 (let ((v_10 (bvnot ((_ int2bv 1) 1))))
 (let ((v_140 (bvnot v_139)))
 (let ((v_141 (bvand (vreg_146 prev) v_140)))
 (let ((v_142
        (bvor
         (ite (= v_10 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_141 ((_ int2bv 1) 0)))))
 (let ((v_143 (bvnot v_142)))
 (let ((v_144 (bvand v_143 ((_ int2bv 1) 1))))
 (let ((v_165 (bvand v_164 v_144)))
 (let ((v_189 (bvnot v_165)))
 (let ((v_190
        (bvor
         (ite (= v_189 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_165 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_191 (bvnot v_190)))
 (let ((v_192 (bvand ((_ int2bv 1) 1) v_191)))
 (let ((v_193 (bvand v_188 v_192)))
 (let ((v_194 (bvand ((_ int2bv 1) 1) v_190)))
 (let ((v_195 (bvor v_193 v_194)))
 (let ((v_246 (bvnot v_245)))
 (let ((v_247 (bvand v_246 ((_ int2bv 1) 1))))
 (let ((v_206 (concat (vinpt_rd_valid_204 inpts) (vinpt_rd_205 inpts))))
 (let ((v_207 (concat (vinpt_rs1_valid_14 inpts) (vinpt_rs1_22 inpts))))
 (let ((v_208 (concat v_206 v_207)))
 (let ((v_209 (concat (vreg_203 prev) v_208)))
 (let ((v_210 (concat (vinpt_rs2_valid_119 inpts) (vinpt_rs2_120 inpts))))
 (let ((v_213
        (concat (vinpt_is_mem_access_211 inpts) (vinpt_can_branch_212 inpts))))
 (let ((v_214 (concat v_210 v_213)))
 (let ((v_215 (concat v_209 v_214)))
 (let ((v_216 (bvor (ite (= v_247 #b1) v_215 ((_ int2bv 14) 0)))))
 (let ((act_166 (bvand ((_ int2bv 1) 1) v_165)))
 (let ((v_6 (bvnot ((_ int2bv 1) 0))))
 (let ((v_7 (bvor (ite (= v_6 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_154 (bvnot v_7)))
 (let ((v_155 (bvand ((_ int2bv 1) 1) v_154)))
 (let ((v_167 (bvand act_166 v_155)))
 (let ((v_168 (bvnot act_166)))
 (let ((v_200
        (ite
         (= v_198 ((_ int2bv 1) 0))
         (vinpt_predicted_pc_199 inpts)
         (vreg_183 prev))))
 (let ((v_201
        (bvor
         (ite (= v_168 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite (= act_166 #b1) v_200 ((_ int2bv 3) 0)))))
 (let ((v_202 (bvor (ite (= v_167 #b1) v_201 ((_ int2bv 3) 0)))))
 (let ((v_145 (bvor (ite (= v_144 #b1) v_164 ((_ int2bv 1) 0)))))
 (let ((v_8 (bvand ((_ int2bv 1) 1) v_7)))
 (let ((v_9 (bvnot act_166)))
 (let ((v_151 (bvnot (vreg_160 prev))))
 (let ((v_152 (bvor v_150 v_151)))
 (let ((v_153 (bvand v_9 v_152)))
 (let ((v_156 (bvand v_153 v_155)))
 (let ((v_157 (bvor v_167 v_156)))
 (let ((v_158 (bvor v_8 v_157)))
 (let ((v_159
        (bvor
         (ite (= v_167 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_156 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_8 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_196
        (bvor
         (ite (= v_193 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_194 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_253 (bvnot v_243)))
 (let ((v_254 (bvnot v_184)))
 (let ((v_255 (bvand v_253 v_254)))
 (let ((v_256 (bvand (vreg_252 prev) v_255)))
 (let ((v_257 (bvand v_256 ((_ int2bv 1) 1))))
 (let ((v_173 ((_ extract 0 0) v_16)))
 (let ((v_175 (bvand v_173 (vinpt_branch_valid_174 inpts))))
 (let ((act_176 (bvand v_175 v_257)))
 (let ((v_177 (bvadd (vreg_172 prev) ((_ int2bv 3) 1))))
 (let ((v_178 (bvnot act_176)))
 (let ((v_180
        (bvor
         (ite (= v_178 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite
          (= act_176 #b1)
          (vinpt_branch_target_179 inpts)
          ((_ int2bv 3) 0)))))
 (let ((v_181 (ite (= act_176 ((_ int2bv 1) 0)) v_177 v_180)))
 (let ((v_182 (bvor (ite (= v_257 #b1) v_181 ((_ int2bv 3) 0)))))
 (let ((v_171 (bvor (ite (= v_247 #b1) (vreg_170 prev) ((_ int2bv 3) 0)))))
 (let ((v_169 (bvor (ite (= v_144 #b1) v_200 ((_ int2bv 3) 0)))))
 (let ((v_37 (bvnot v_110)))
 (let ((v_38 (bvand v_37 ((_ int2bv 1) 1))))
 (let ((v_39 (bvnot v_240)))
 (let ((v_40 (bvand (vreg_5 prev) v_39)))
 (let ((v_41 (bvor (ite (= v_38 #b1) v_40 ((_ int2bv 1) 0)))))
 (let ((v_43 ((_ extract 8 6) v_27)))
 (let ((v_44 (concat v_30 v_31)))
 (let ((v_45 ((_ extract 2 0) v_28)))
 (let ((v_46 ((_ extract 2 2) v_45)))
 (let ((v_47 ((_ extract 1 0) v_45)))
 (let ((v_48 (concat v_46 v_47)))
 (let ((v_49 (concat v_44 v_48)))
 (let ((v_50 (concat v_43 v_49)))
 (let ((v_51 ((_ extract 4 2) v_236)))
 (let ((v_52 ((_ extract 2 2) v_51)))
 (let ((v_53 ((_ extract 1 0) v_51)))
 (let ((v_54 (concat v_52 v_53)))
 (let ((v_55 ((_ extract 0 0) v_237)))
 (let ((v_56 (concat v_238 v_55)))
 (let ((v_57 (concat v_54 v_56)))
 (let ((v_58 (concat v_50 v_57)))
 (let ((v_59 (bvor (ite (= v_38 #b1) v_58 ((_ int2bv 14) 0)))))
 (let ((v_68 (bvnot ((_ int2bv 1) 0))))
 (let ((v_69 (bvor (ite (= v_68 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_70 (bvand ((_ int2bv 1) 1) v_69)))
 (let ((v_76 ((_ extract 0 0) (vreg_75 prev))))
 (let ((v_88 (bvand v_238 v_87)))
 (let ((v_89 (bvand (vreg_5 prev) v_88)))
 (let ((v_90 (bvand v_89 v_38)))
 (let ((act_91 (bvand v_76 v_90)))
 (let ((v_92 (bvnot v_69)))
 (let ((v_93 (bvand ((_ int2bv 1) 1) v_92)))
 (let ((v_94 (bvand act_91 v_93)))
 (let ((v_95 (bvnot act_91)))
 (let ((v_96 (bvnot (vreg_103 prev))))
 (let ((v_97 (bvor v_84 v_96)))
 (let ((v_98 (bvand v_95 v_97)))
 (let ((v_99 (bvand v_98 v_93)))
 (let ((v_100 (bvor v_94 v_99)))
 (let ((v_101 (bvor v_70 v_100)))
 (let ((v_71 (bvand v_17 v_257)))
 (let ((v_73 (concat (vinpt_mreq_val_72 inpts) v_20)))
 (let ((v_74 (bvor (ite (= v_71 #b1) v_73 ((_ int2bv 4) 0)))))
 (let ((v_102
        (bvor
         (ite (= v_94 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_99 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_70 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_2 (bvnot v_243)))
 (let ((v_3 (bvand v_2 ((_ int2bv 1) 1))))
 (let ((v_219 ((_ extract 8 6) v_218)))
 (let ((v_220 (concat v_20 v_21)))
 (let ((v_221 ((_ extract 2 0) v_18)))
 (let ((v_222 ((_ extract 2 2) v_221)))
 (let ((v_223 ((_ extract 1 0) v_221)))
 (let ((v_224 (concat v_222 v_223)))
 (let ((v_225 (concat v_220 v_224)))
 (let ((v_226 (concat v_219 v_225)))
 (let ((v_227 ((_ extract 4 2) v_15)))
 (let ((v_228 ((_ extract 2 2) v_227)))
 (let ((v_229 ((_ extract 1 0) v_227)))
 (let ((v_230 (concat v_228 v_229)))
 (let ((v_231 (concat v_17 v_173)))
 (let ((v_232 (concat v_230 v_231)))
 (let ((v_233 (concat v_226 v_232)))
 (let ((v_234 (bvor (ite (= v_3 #b1) v_233 ((_ int2bv 14) 0)))))
 (let ((v_4 (bvor (ite (= v_3 #b1) v_256 ((_ int2bv 1) 0)))))
 (let ((v_248 (bvnot v_198)))
 (let ((v_249 (bvand v_139 v_248)))
 (let ((v_250 (bvand (vreg_146 prev) v_249)))
 (let ((v_251 (bvor (ite (= v_247 #b1) v_250 ((_ int2bv 1) 0)))))
 (let ((v_366 (bvadd (vreg_368 prev) ((_ int2bv 2) 1))))
 (let ((v_367
        (bvor
         (ite (= v_363 #b1) ((_ int2bv 2) 0) ((_ int2bv 2) 0))
         (ite (= v_364 #b1) v_366 ((_ int2bv 2) 0)))))
 (mkTuple2
  (=> (= ((_ int2bv 1) 1) #b1) (= v_369 #b1))
  (mkState_assert_v_369
   (ite (= v_3 #b1) v_4 (vreg_5 prev))
   (ite (= v_38 #b1) v_41 (vreg_42 prev))
   (ite (= v_38 #b1) v_59 (vreg_60 prev))
   (ite (= v_71 #b1) v_74 (vreg_75 prev))
   (ite (= v_101 #b1) v_102 (vreg_103 prev))
   (ite (= v_144 #b1) v_145 (vreg_146 prev))
   (ite (= v_158 #b1) v_159 (vreg_160 prev))
   (ite (= v_144 #b1) v_169 (vreg_170 prev))
   (ite (= v_247 #b1) v_171 (vreg_172 prev))
   (ite (= v_257 #b1) v_182 (vreg_183 prev))
   (ite (= v_195 #b1) v_196 (vreg_197 prev))
   (ite (= v_167 #b1) v_202 (vreg_203 prev))
   (ite (= v_247 #b1) v_216 (vreg_217 prev))
   (ite (= v_3 #b1) v_234 (vreg_235 prev))
   (ite (= v_247 #b1) v_251 (vreg_252 prev))
   (ite
    (= v_365 #b1)
    v_367
    (vreg_368
     prev))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;-------------------------------------------------------------------------------
; Defining the specific chaining of the transition function
(define-fun-rec
 chain_tFun_assert_v_369
 ((xs (ListX Input_assert_v_369)) (prev State_assert_v_369))
 (Tuple2 (ListX Bool) (ListX State_assert_v_369))
 (match
  xs
  ((nil
    (mkTuple2
     (as nil (ListX Bool))
     (cons prev (as nil (ListX State_assert_v_369)))))
   ((cons h t)
    (match
     (tFun_assert_v_369 h prev)
     (((mkTuple2 ret next)
       (match
        (chain_tFun_assert_v_369 t next)
        (((mkTuple2 rets ys) (mkTuple2 (cons ret rets) (cons prev ys))))))))))))
(push)
;-------------------------------------------------------------------------------
(echo "No consecutive branch mispredictions")
(echo "--------------------------------------------------------------------------------")
(echo "Using bounded verification of depth 7")
(echo "Bounded property refutation:")
(declare-const in0 Input_assert_v_369)
(declare-const in1 Input_assert_v_369)
(declare-const in2 Input_assert_v_369)
(declare-const in3 Input_assert_v_369)
(declare-const in4 Input_assert_v_369)
(declare-const in5 Input_assert_v_369)
(declare-const in6 Input_assert_v_369)
(assert
 (let ((s0
        (mkState_assert_v_369
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 4) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 2) 0)))
       (inpts
        (cons
         in0
         (cons
          in1
          (cons
           in2
           (cons
            in3
            (cons
             in4
             (cons in5 (cons in6 (as nil (ListX Input_assert_v_369)))))))))))
      (match
       (match
        (tFun_assert_v_369 in0 s0)
        (((mkTuple2 ret0 s1)
          (match
           (tFun_assert_v_369 in1 s1)
           (((mkTuple2 ret1 s2)
             (match
              (tFun_assert_v_369 in2 s2)
              (((mkTuple2 ret2 s3)
                (match
                 (tFun_assert_v_369 in3 s3)
                 (((mkTuple2 ret3 s4)
                   (match
                    (tFun_assert_v_369 in4 s4)
                    (((mkTuple2 ret4 s5)
                      (match
                       (tFun_assert_v_369 in5 s5)
                       (((mkTuple2 ret5 s6)
                         (match
                          (tFun_assert_v_369 in6 s6)
                          (((mkTuple2 ret6 s7)
                            (mkTuple2
                             (cons
                              ret0
                              (cons
                               ret1
                               (cons
                                ret2
                                (cons
                                 ret3
                                 (cons
                                  ret4
                                  (cons
                                   ret5
                                   (cons ret6 (as nil (ListX Bool)))))))))
                             (cons
                              s0
                              (cons
                               s1
                               (cons
                                s2
                                (cons
                                 s3
                                 (cons
                                  s4
                                  (cons
                                   s5
                                   (cons
                                    s6
                                    (as
                                     nil
                                     (ListX State_assert_v_369)))))))))))))))))))))))))))))))
       (((mkTuple2 oks ss) (not (andReduce oks)))))))
(check-sat)
(pop)