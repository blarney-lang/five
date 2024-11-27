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
; code gen for "assert_v_291"
;-------------------------------------------------------------------------------
(push)
;-------------------------------------------------------------------------------
; Defining the specific Inputs record type
(declare-datatype
 Input_assert_v_291
 ((mkInput_assert_v_291
   (vinpt_imem_peek_mask_32 (_ BitVec 1))
   (vinpt_rs1_valid_49 (_ BitVec 1))
   (vinpt_branch_valid_57 (_ BitVec 1))
   (vinpt_branch_target_62 (_ BitVec 3))
   (vinpt_predicted_pc_82 (_ BitVec 3))
   (vinpt_rd_valid_87 (_ BitVec 1))
   (vinpt_rd_88 (_ BitVec 2))
   (vinpt_rs1_90 (_ BitVec 2))
   (vinpt_rs2_valid_94 (_ BitVec 1))
   (vinpt_rs2_95 (_ BitVec 2))
   (vinpt_is_mem_access_97 (_ BitVec 1))
   (vinpt_can_branch_98 (_ BitVec 1))
   (vinpt_mreq_val_180 (_ BitVec 3))
   (vinpt_dmem_peek_mask_257 (_ BitVec 1)))))
;-------------------------------------------------------------------------------
; Defining the specific State record type
(declare-datatype
 State_assert_v_291
 ((mkState_assert_v_291
   (vreg_31 (_ BitVec 1))
   (vreg_42 (_ BitVec 2))
   (vreg_52 (_ BitVec 3))
   (vreg_54 (_ BitVec 3))
   (vreg_66 (_ BitVec 3))
   (vreg_80 (_ BitVec 1))
   (vreg_86 (_ BitVec 3))
   (vreg_103 (_ BitVec 14))
   (vreg_164 (_ BitVec 1))
   (vreg_169 (_ BitVec 1))
   (vreg_175 (_ BitVec 1))
   (vreg_183 (_ BitVec 4))
   (vreg_207 (_ BitVec 1))
   (vreg_234 (_ BitVec 14))
   (vreg_253 (_ BitVec 14))
   (vreg_267 (_ BitVec 2))
   (vreg_283 (_ BitVec 1))
   (vreg_290 (_ BitVec 5)))))
;-------------------------------------------------------------------------------
; Defining the specific transition function
(define-fun
 tFun_assert_v_291
 ((inpts Input_assert_v_291) (prev State_assert_v_291))
 (Tuple2 Bool State_assert_v_291)
 (let ((v_291 (ite (bvule (vreg_290 prev) ((_ int2bv 5) 10)) #b1 #b0)))
 (let ((v_284 (bvnot (vreg_283 prev))))
 (let ((v_285 (bvand ((_ int2bv 1) 1) v_284)))
 (let ((v_286 (bvand ((_ int2bv 1) 1) (vreg_283 prev))))
 (let ((v_287 (bvor v_285 v_286)))
 (let ((v_0 (bvnot ((_ int2bv 1) 1))))
 (let ((v_254 ((_ extract 4 0) (vreg_253 prev))))
 (let ((v_255 ((_ extract 1 0) v_254)))
 (let ((v_256 ((_ extract 1 1) v_255)))
 (let ((v_127 ((_ extract 13 5) (vreg_253 prev))))
 (let ((v_128 ((_ extract 5 0) v_127)))
 (let ((v_129 ((_ extract 5 3) v_128)))
 (let ((v_130 ((_ extract 2 2) v_129)))
 (let ((v_258 (bvnot (vinpt_dmem_peek_mask_257 inpts))))
 (let ((v_259 (bvand ((_ int2bv 1) 1) v_258)))
 (let ((v_268 (ite (= (vreg_267 prev) ((_ int2bv 2) 1)) #b1 #b0)))
 (let ((v_269 (bvand v_259 v_268)))
 (let ((v_270 (bvnot v_269)))
 (let ((v_271
        (bvor
         (ite (= v_270 #b1) (vinpt_dmem_peek_mask_257 inpts) ((_ int2bv 1) 0))
         (ite (= v_269 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_272 (bvand (vreg_207 prev) v_271)))
 (let ((v_273 (bvnot v_272)))
 (let ((v_274 (bvand v_130 v_273)))
 (let ((v_275 (bvand v_256 v_274)))
 (let ((v_276 (bvand (vreg_283 prev) v_275)))
 (let ((v_277
        (bvor
         (ite (= v_0 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_276 ((_ int2bv 1) 0)))))
 (let ((v_278 (bvnot v_277)))
 (let ((v_279 (bvand v_278 ((_ int2bv 1) 1))))
 (let ((v_260 (bvand ((_ int2bv 1) 1) (vinpt_dmem_peek_mask_257 inpts))))
 (let ((v_261 (bvnot v_268)))
 (let ((v_262 (bvand v_259 v_261)))
 (let ((v_263 (bvor v_262 v_269)))
 (let ((v_264 (bvor v_260 v_263)))
 (let ((v_265 (bvadd (vreg_267 prev) ((_ int2bv 2) 1))))
 (let ((v_266
        (bvor
         (ite (= v_262 #b1) v_265 ((_ int2bv 2) 0))
         (ite (= v_269 #b1) ((_ int2bv 2) 0) ((_ int2bv 2) 0))
         (ite (= v_260 #b1) ((_ int2bv 2) 0) ((_ int2bv 2) 0)))))
 (let ((v_176 (bvnot ((_ int2bv 1) 0))))
 (let ((v_177 (bvor (ite (= v_176 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_178 (bvand ((_ int2bv 1) 1) v_177)))
 (let ((v_184 ((_ extract 0 0) (vreg_183 prev))))
 (let ((v_116 ((_ extract 4 0) (vreg_234 prev))))
 (let ((v_117 ((_ extract 1 0) v_116)))
 (let ((v_118 ((_ extract 1 1) v_117)))
 (let ((v_208 (bvnot (vreg_207 prev))))
 (let ((v_193 (bvnot v_275)))
 (let ((v_194 (bvand v_130 v_193)))
 (let ((v_195 (bvand (vreg_283 prev) v_194)))
 (let ((v_196 (bvand v_195 ((_ int2bv 1) 1))))
 (let ((v_197 (bvand v_256 v_196)))
 (let ((v_198 (bvnot v_197)))
 (let ((v_199
        (bvor
         (ite (= v_198 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_197 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_209 (bvor v_208 v_199)))
 (let ((v_210 (bvand v_209 ((_ int2bv 1) 1))))
 (let ((v_185 (bvand v_118 v_210)))
 (let ((v_186 (bvand (vreg_175 prev) v_185)))
 (let ((v_187 (bvand v_186 v_279)))
 (let ((act_188 (bvand v_184 v_187)))
 (let ((v_189 (bvnot v_177)))
 (let ((v_190 (bvand ((_ int2bv 1) 1) v_189)))
 (let ((v_191 (bvand act_188 v_190)))
 (let ((v_192 (bvnot act_188)))
 (let ((v_200 (bvnot (vreg_207 prev))))
 (let ((v_201 (bvor v_199 v_200)))
 (let ((v_202 (bvand v_192 v_201)))
 (let ((v_203 (bvand v_202 v_190)))
 (let ((v_204 (bvor v_191 v_203)))
 (let ((v_205 (bvor v_178 v_204)))
 (let ((v_1 (bvnot ((_ int2bv 1) 1))))
 (let ((v_211 (bvnot v_210)))
 (let ((v_212 (bvand v_118 v_211)))
 (let ((v_213 (bvor v_212 v_277)))
 (let ((v_214 (bvand (vreg_175 prev) v_213)))
 (let ((v_215
        (bvor
         (ite (= v_1 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_214 ((_ int2bv 1) 0)))))
 (let ((v_216 (bvnot v_215)))
 (let ((v_217 (bvand v_216 ((_ int2bv 1) 1))))
 (let ((v_107 ((_ extract 13 5) (vreg_103 prev))))
 (let ((v_218 ((_ extract 8 6) v_107)))
 (let ((v_108 ((_ extract 5 0) v_107)))
 (let ((v_109 ((_ extract 5 3) v_108)))
 (let ((v_110 ((_ extract 2 2) v_109)))
 (let ((v_111 ((_ extract 1 0) v_109)))
 (let ((v_219 (concat v_110 v_111)))
 (let ((v_220 ((_ extract 2 0) v_108)))
 (let ((v_221 ((_ extract 2 2) v_220)))
 (let ((v_222 ((_ extract 1 0) v_220)))
 (let ((v_223 (concat v_221 v_222)))
 (let ((v_224 (concat v_219 v_223)))
 (let ((v_225 (concat v_218 v_224)))
 (let ((v_104 ((_ extract 4 0) (vreg_103 prev))))
 (let ((v_226 ((_ extract 4 2) v_104)))
 (let ((v_227 ((_ extract 2 2) v_226)))
 (let ((v_228 ((_ extract 1 0) v_226)))
 (let ((v_229 (concat v_227 v_228)))
 (let ((v_105 ((_ extract 1 0) v_104)))
 (let ((v_106 ((_ extract 1 1) v_105)))
 (let ((v_56 ((_ extract 0 0) v_105)))
 (let ((v_230 (concat v_106 v_56)))
 (let ((v_231 (concat v_229 v_230)))
 (let ((v_232 (concat v_225 v_231)))
 (let ((v_233 (bvor (ite (= v_217 #b1) v_232 ((_ int2bv 14) 0)))))
 (let ((v_2 (bvnot ((_ int2bv 1) 1))))
 (let ((v_3 (bvand (vreg_169 prev) v_215)))
 (let ((v_4
        (bvor
         (ite (= v_2 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_3 ((_ int2bv 1) 0)))))
 (let ((v_5 (bvnot v_4)))
 (let ((v_6 (bvand v_5 ((_ int2bv 1) 1))))
 (let ((v_33 (bvnot (vinpt_imem_peek_mask_32 inpts))))
 (let ((v_34 (bvand ((_ int2bv 1) 1) v_33)))
 (let ((v_43 (ite (= (vreg_42 prev) ((_ int2bv 2) 1)) #b1 #b0)))
 (let ((v_44 (bvand v_34 v_43)))
 (let ((v_45 (bvnot v_44)))
 (let ((v_46
        (bvor
         (ite (= v_45 #b1) (vinpt_imem_peek_mask_32 inpts) ((_ int2bv 1) 0))
         (ite (= v_44 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_47 (bvand (vreg_31 prev) v_46)))
 (let ((v_48 (bvnot v_4)))
 (let ((v_112 (ite (= v_111 (vinpt_rs1_90 inpts)) #b1 #b0)))
 (let ((v_113 (bvand v_110 v_112)))
 (let ((v_114 (bvand v_106 v_113)))
 (let ((v_115 (bvand (vreg_169 prev) v_114)))
 (let ((v_235 ((_ extract 13 5) (vreg_234 prev))))
 (let ((v_119 ((_ extract 5 0) v_235)))
 (let ((v_120 ((_ extract 5 3) v_119)))
 (let ((v_121 ((_ extract 2 2) v_120)))
 (let ((v_122 ((_ extract 1 0) v_120)))
 (let ((v_123 (ite (= v_122 (vinpt_rs1_90 inpts)) #b1 #b0)))
 (let ((v_124 (bvand v_121 v_123)))
 (let ((v_125 (bvand v_118 v_124)))
 (let ((v_126 (bvand (vreg_175 prev) v_125)))
 (let ((v_131 ((_ extract 1 0) v_129)))
 (let ((v_132 (ite (= v_131 (vinpt_rs1_90 inpts)) #b1 #b0)))
 (let ((v_133 (bvand v_130 v_132)))
 (let ((v_134 (bvand v_256 v_133)))
 (let ((v_135 (bvand v_277 v_134)))
 (let ((v_136 (bvor v_126 v_135)))
 (let ((v_137 (bvor v_115 v_136)))
 (let ((v_138 (bvand (vinpt_rs1_valid_49 inpts) v_137)))
 (let ((v_139 (ite (= v_111 (vinpt_rs2_95 inpts)) #b1 #b0)))
 (let ((v_140 (bvand v_110 v_139)))
 (let ((v_141 (bvand v_106 v_140)))
 (let ((v_142 (bvand (vreg_169 prev) v_141)))
 (let ((v_143 (ite (= v_122 (vinpt_rs2_95 inpts)) #b1 #b0)))
 (let ((v_144 (bvand v_121 v_143)))
 (let ((v_145 (bvand v_118 v_144)))
 (let ((v_146 (bvand (vreg_175 prev) v_145)))
 (let ((v_147 (ite (= v_131 (vinpt_rs2_95 inpts)) #b1 #b0)))
 (let ((v_148 (bvand v_130 v_147)))
 (let ((v_149 (bvand v_256 v_148)))
 (let ((v_150 (bvand v_277 v_149)))
 (let ((v_151 (bvor v_146 v_150)))
 (let ((v_152 (bvor v_142 v_151)))
 (let ((v_153 (bvand (vinpt_rs2_valid_94 inpts) v_152)))
 (let ((v_154 (bvor v_138 v_153)))
 (let ((v_155 (bvnot v_154)))
 (let ((v_156 (bvand v_48 v_155)))
 (let ((v_157 (bvand v_47 v_156)))
 (let ((v_67 (bvnot (ite (= (vreg_54 prev) (vreg_66 prev)) #b1 #b0))))
 (let ((v_68 (bvand (vreg_169 prev) v_67)))
 (let ((v_69 (bvand v_68 ((_ int2bv 1) 1))))
 (let ((v_70 (bvnot v_69)))
 (let ((v_71
        (bvor
         (ite (= v_70 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_69 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_81 (bvor v_71 (vreg_80 prev))))
 (let ((v_165 (bvnot v_81)))
 (let ((v_166 (bvand v_157 v_165)))
 (let ((v_167 (bvand (vreg_164 prev) v_166)))
 (let ((v_168 (bvor (ite (= v_6 #b1) v_167 ((_ int2bv 1) 0)))))
 (let ((v_11 (bvnot (vreg_31 prev))))
 (let ((v_12 (bvand (vreg_164 prev) v_157)))
 (let ((v_13 (bvand v_12 ((_ int2bv 1) 1))))
 (let ((v_14 (bvnot v_13)))
 (let ((v_15
        (bvor
         (ite (= v_14 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_13 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_16 (bvor v_11 v_15)))
 (let ((v_17 (bvand v_16 ((_ int2bv 1) 1))))
 (let ((v_7 (bvnot ((_ int2bv 1) 1))))
 (let ((v_158 (bvnot v_157)))
 (let ((v_159 (bvand (vreg_164 prev) v_158)))
 (let ((v_160
        (bvor
         (ite (= v_7 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= ((_ int2bv 1) 1) #b1) v_159 ((_ int2bv 1) 0)))))
 (let ((v_161 (bvnot v_160)))
 (let ((v_162 (bvand v_161 ((_ int2bv 1) 1))))
 (let ((v_18 (bvand v_17 v_162)))
 (let ((v_72 (bvnot v_18)))
 (let ((v_73
        (bvor
         (ite (= v_72 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_18 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0)))))
 (let ((v_74 (bvnot v_73)))
 (let ((v_75 (bvand ((_ int2bv 1) 1) v_74)))
 (let ((v_76 (bvand v_71 v_75)))
 (let ((v_77 (bvand ((_ int2bv 1) 1) v_73)))
 (let ((v_78 (bvor v_76 v_77)))
 (let ((v_79
        (bvor
         (ite (= v_76 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_77 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_170 (bvnot v_215)))
 (let ((v_171 (bvnot v_67)))
 (let ((v_172 (bvand v_170 v_171)))
 (let ((v_173 (bvand (vreg_169 prev) v_172)))
 (let ((v_55 (bvand v_173 ((_ int2bv 1) 1))))
 (let ((v_58 (bvand v_56 (vinpt_branch_valid_57 inpts))))
 (let ((act_59 (bvand v_58 v_55)))
 (let ((v_60 (bvadd (vreg_54 prev) ((_ int2bv 3) 1))))
 (let ((v_61 (bvnot act_59)))
 (let ((v_63
        (bvor
         (ite (= v_61 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite (= act_59 #b1) (vinpt_branch_target_62 inpts) ((_ int2bv 3) 0)))))
 (let ((v_64 (ite (= act_59 ((_ int2bv 1) 0)) v_60 v_63)))
 (let ((v_65 (bvor (ite (= v_55 #b1) v_64 ((_ int2bv 3) 0)))))
 (let ((v_53 (bvor (ite (= v_6 #b1) (vreg_52 prev) ((_ int2bv 3) 0)))))
 (let ((v_83
        (ite
         (= v_81 ((_ int2bv 1) 0))
         (vinpt_predicted_pc_82 inpts)
         (vreg_66 prev))))
 (let ((v_51 (bvor (ite (= v_162 #b1) v_83 ((_ int2bv 3) 0)))))
 (let ((v_35 (bvand ((_ int2bv 1) 1) (vinpt_imem_peek_mask_32 inpts))))
 (let ((v_36 (bvnot v_43)))
 (let ((v_37 (bvand v_34 v_36)))
 (let ((v_38 (bvor v_37 v_44)))
 (let ((v_39 (bvor v_35 v_38)))
 (let ((v_40 (bvadd (vreg_42 prev) ((_ int2bv 2) 1))))
 (let ((v_41
        (bvor
         (ite (= v_37 #b1) v_40 ((_ int2bv 2) 0))
         (ite (= v_44 #b1) ((_ int2bv 2) 0) ((_ int2bv 2) 0))
         (ite (= v_35 #b1) ((_ int2bv 2) 0) ((_ int2bv 2) 0)))))
 (let ((v_8 (bvnot ((_ int2bv 1) 0))))
 (let ((v_9 (bvor (ite (= v_8 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_10 (bvand ((_ int2bv 1) 1) v_9)))
 (let ((act_19 (bvand ((_ int2bv 1) 1) v_18)))
 (let ((v_20 (bvnot v_9)))
 (let ((v_21 (bvand ((_ int2bv 1) 1) v_20)))
 (let ((v_22 (bvand act_19 v_21)))
 (let ((v_23 (bvnot act_19)))
 (let ((v_24 (bvnot (vreg_31 prev))))
 (let ((v_25 (bvor v_15 v_24)))
 (let ((v_26 (bvand v_23 v_25)))
 (let ((v_27 (bvand v_26 v_21)))
 (let ((v_28 (bvor v_22 v_27)))
 (let ((v_29 (bvor v_10 v_28)))
 (let ((v_30
        (bvor
         (ite (= v_22 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_27 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_10 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_163 (bvor (ite (= v_162 #b1) v_17 ((_ int2bv 1) 0)))))
 (let ((v_89 (concat (vinpt_rd_valid_87 inpts) (vinpt_rd_88 inpts))))
 (let ((v_91 (concat (vinpt_rs1_valid_49 inpts) (vinpt_rs1_90 inpts))))
 (let ((v_92 (concat v_89 v_91)))
 (let ((v_93 (concat (vreg_86 prev) v_92)))
 (let ((v_96 (concat (vinpt_rs2_valid_94 inpts) (vinpt_rs2_95 inpts))))
 (let ((v_99
        (concat (vinpt_is_mem_access_97 inpts) (vinpt_can_branch_98 inpts))))
 (let ((v_100 (concat v_96 v_99)))
 (let ((v_101 (concat v_93 v_100)))
 (let ((v_102 (bvor (ite (= v_6 #b1) v_101 ((_ int2bv 14) 0)))))
 (let ((v_50 (bvnot act_19)))
 (let ((v_84
        (bvor
         (ite (= v_50 #b1) ((_ int2bv 3) 0) ((_ int2bv 3) 0))
         (ite (= act_19 #b1) v_83 ((_ int2bv 3) 0)))))
 (let ((v_85 (bvor (ite (= v_22 #b1) v_84 ((_ int2bv 3) 0)))))
 (let ((v_174 (bvor (ite (= v_217 #b1) v_173 ((_ int2bv 1) 0)))))
 (let ((v_179 (bvand v_106 v_55)))
 (let ((v_181 (concat (vinpt_mreq_val_180 inpts) v_110)))
 (let ((v_182 (bvor (ite (= v_179 #b1) v_181 ((_ int2bv 4) 0)))))
 (let ((v_206
        (bvor
         (ite (= v_191 #b1) ((_ int2bv 1) 1) ((_ int2bv 1) 0))
         (ite (= v_203 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0))
         (ite (= v_178 #b1) ((_ int2bv 1) 0) ((_ int2bv 1) 0)))))
 (let ((v_236 ((_ extract 8 6) v_235)))
 (let ((v_237 (concat v_121 v_122)))
 (let ((v_238 ((_ extract 2 0) v_119)))
 (let ((v_239 ((_ extract 2 2) v_238)))
 (let ((v_240 ((_ extract 1 0) v_238)))
 (let ((v_241 (concat v_239 v_240)))
 (let ((v_242 (concat v_237 v_241)))
 (let ((v_243 (concat v_236 v_242)))
 (let ((v_244 ((_ extract 4 2) v_116)))
 (let ((v_245 ((_ extract 2 2) v_244)))
 (let ((v_246 ((_ extract 1 0) v_244)))
 (let ((v_247 (concat v_245 v_246)))
 (let ((v_248 ((_ extract 0 0) v_117)))
 (let ((v_249 (concat v_118 v_248)))
 (let ((v_250 (concat v_247 v_249)))
 (let ((v_251 (concat v_243 v_250)))
 (let ((v_252 (bvor (ite (= v_279 #b1) v_251 ((_ int2bv 14) 0)))))
 (let ((v_280 (bvnot v_212)))
 (let ((v_281 (bvand (vreg_175 prev) v_280)))
 (let ((v_282 (bvor (ite (= v_279 #b1) v_281 ((_ int2bv 1) 0)))))
 (let ((v_288 (bvadd (vreg_290 prev) ((_ int2bv 5) 1))))
 (let ((v_289
        (bvor
         (ite (= v_285 #b1) v_288 ((_ int2bv 5) 0))
         (ite (= v_286 #b1) ((_ int2bv 5) 0) ((_ int2bv 5) 0)))))
 (mkTuple2
  (=> (= ((_ int2bv 1) 1) #b1) (= v_291 #b1))
  (mkState_assert_v_291
   (ite (= v_29 #b1) v_30 (vreg_31 prev))
   (ite (= v_39 #b1) v_41 (vreg_42 prev))
   (ite (= v_162 #b1) v_51 (vreg_52 prev))
   (ite (= v_6 #b1) v_53 (vreg_54 prev))
   (ite (= v_55 #b1) v_65 (vreg_66 prev))
   (ite (= v_78 #b1) v_79 (vreg_80 prev))
   (ite (= v_22 #b1) v_85 (vreg_86 prev))
   (ite (= v_6 #b1) v_102 (vreg_103 prev))
   (ite (= v_162 #b1) v_163 (vreg_164 prev))
   (ite (= v_6 #b1) v_168 (vreg_169 prev))
   (ite (= v_217 #b1) v_174 (vreg_175 prev))
   (ite (= v_179 #b1) v_182 (vreg_183 prev))
   (ite (= v_205 #b1) v_206 (vreg_207 prev))
   (ite (= v_217 #b1) v_233 (vreg_234 prev))
   (ite (= v_279 #b1) v_252 (vreg_253 prev))
   (ite (= v_264 #b1) v_266 (vreg_267 prev))
   (ite (= v_279 #b1) v_282 (vreg_283 prev))
   (ite
    (= v_287 #b1)
    v_289
    (vreg_290
     prev)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;-------------------------------------------------------------------------------
; Defining the specific chaining of the transition function
(define-fun-rec
 chain_tFun_assert_v_291
 ((xs (ListX Input_assert_v_291)) (prev State_assert_v_291))
 (Tuple2 (ListX Bool) (ListX State_assert_v_291))
 (match
  xs
  ((nil
    (mkTuple2
     (as nil (ListX Bool))
     (cons prev (as nil (ListX State_assert_v_291)))))
   ((cons h t)
    (match
     (tFun_assert_v_291 h prev)
     (((mkTuple2 ret next)
       (match
        (chain_tFun_assert_v_291 t next)
        (((mkTuple2 rets ys) (mkTuple2 (cons ret rets) (cons prev ys))))))))))))
(push)
;-------------------------------------------------------------------------------
(echo "Forward progress")
(echo "--------------------------------------------------------------------------------")
(echo "Using bounded verification of depth 22")
(echo "Bounded property refutation:")
(declare-const in0 Input_assert_v_291)
(declare-const in1 Input_assert_v_291)
(declare-const in2 Input_assert_v_291)
(declare-const in3 Input_assert_v_291)
(declare-const in4 Input_assert_v_291)
(declare-const in5 Input_assert_v_291)
(declare-const in6 Input_assert_v_291)
(declare-const in7 Input_assert_v_291)
(declare-const in8 Input_assert_v_291)
(declare-const in9 Input_assert_v_291)
(declare-const in10 Input_assert_v_291)
(declare-const in11 Input_assert_v_291)
(declare-const in12 Input_assert_v_291)
(declare-const in13 Input_assert_v_291)
(declare-const in14 Input_assert_v_291)
(declare-const in15 Input_assert_v_291)
(declare-const in16 Input_assert_v_291)
(declare-const in17 Input_assert_v_291)
(declare-const in18 Input_assert_v_291)
(declare-const in19 Input_assert_v_291)
(declare-const in20 Input_assert_v_291)
(declare-const in21 Input_assert_v_291)
(assert
 (let ((s0
        (mkState_assert_v_291
         ((_ int2bv 1) 0)
         ((_ int2bv 2) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 3) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 4) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 14) 0)
         ((_ int2bv 2) 0)
         ((_ int2bv 1) 0)
         ((_ int2bv 5) 0)))
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
             (cons
              in5
              (cons
               in6
               (cons
                in7
                (cons
                 in8
                 (cons
                  in9
                  (cons
                   in10
                   (cons
                    in11
                    (cons
                     in12
                     (cons
                      in13
                      (cons
                       in14
                       (cons
                        in15
                        (cons
                         in16
                         (cons
                          in17
                          (cons
                           in18
                           (cons
                            in19
                            (cons
                             in20
                             (cons
                              in21
                              (as
                               nil
                               (ListX Input_assert_v_291))))))))))))))))))))))))))
      (match
       (match
        (tFun_assert_v_291 in0 s0)
        (((mkTuple2 ret0 s1)
          (match
           (tFun_assert_v_291 in1 s1)
           (((mkTuple2 ret1 s2)
             (match
              (tFun_assert_v_291 in2 s2)
              (((mkTuple2 ret2 s3)
                (match
                 (tFun_assert_v_291 in3 s3)
                 (((mkTuple2 ret3 s4)
                   (match
                    (tFun_assert_v_291 in4 s4)
                    (((mkTuple2 ret4 s5)
                      (match
                       (tFun_assert_v_291 in5 s5)
                       (((mkTuple2 ret5 s6)
                         (match
                          (tFun_assert_v_291 in6 s6)
                          (((mkTuple2 ret6 s7)
                            (match
                             (tFun_assert_v_291 in7 s7)
                             (((mkTuple2 ret7 s8)
                               (match
                                (tFun_assert_v_291 in8 s8)
                                (((mkTuple2 ret8 s9)
                                  (match
                                   (tFun_assert_v_291 in9 s9)
                                   (((mkTuple2 ret9 s10)
                                     (match
                                      (tFun_assert_v_291 in10 s10)
                                      (((mkTuple2 ret10 s11)
                                        (match
                                         (tFun_assert_v_291 in11 s11)
                                         (((mkTuple2 ret11 s12)
                                           (match
                                            (tFun_assert_v_291 in12 s12)
                                            (((mkTuple2 ret12 s13)
                                              (match
                                               (tFun_assert_v_291 in13 s13)
                                               (((mkTuple2 ret13 s14)
                                                 (match
                                                  (tFun_assert_v_291 in14 s14)
                                                  (((mkTuple2 ret14 s15)
                                                    (match
                                                     (tFun_assert_v_291
                                                      in15
                                                      s15)
                                                     (((mkTuple2 ret15 s16)
                                                       (match
                                                        (tFun_assert_v_291
                                                         in16
                                                         s16)
                                                        (((mkTuple2 ret16 s17)
                                                          (match
                                                           (tFun_assert_v_291
                                                            in17
                                                            s17)
                                                           (((mkTuple2 ret17 s18)
                                                             (match
                                                              (tFun_assert_v_291
                                                               in18
                                                               s18)
                                                              (((mkTuple2 ret18 s19)
                                                                (match
                                                                 (tFun_assert_v_291
                                                                  in19
                                                                  s19)
                                                                 (((mkTuple2 ret19 s20)
                                                                   (match
                                                                    (tFun_assert_v_291
                                                                     in20
                                                                     s20)
                                                                    (((mkTuple2 ret20 s21)
                                                                      (match
                                                                       (tFun_assert_v_291
                                                                        in21
                                                                        s21)
                                                                       (((mkTuple2 ret21 s22)
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
                                                                                (cons
                                                                                 ret6
                                                                                 (cons
                                                                                  ret7
                                                                                  (cons
                                                                                   ret8
                                                                                   (cons
                                                                                    ret9
                                                                                    (cons
                                                                                     ret10
                                                                                     (cons
                                                                                      ret11
                                                                                      (cons
                                                                                       ret12
                                                                                       (cons
                                                                                        ret13
                                                                                        (cons
                                                                                         ret14
                                                                                         (cons
                                                                                          ret15
                                                                                          (cons
                                                                                           ret16
                                                                                           (cons
                                                                                            ret17
                                                                                            (cons
                                                                                             ret18
                                                                                             (cons
                                                                                              ret19
                                                                                              (cons
                                                                                               ret20
                                                                                               (cons
                                                                                                ret21
                                                                                                (as
                                                                                                 nil
                                                                                                 (ListX Bool))))))))))))))))))))))))
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
                                                                                 (cons
                                                                                  s7
                                                                                  (cons
                                                                                   s8
                                                                                   (cons
                                                                                    s9
                                                                                    (cons
                                                                                     s10
                                                                                     (cons
                                                                                      s11
                                                                                      (cons
                                                                                       s12
                                                                                       (cons
                                                                                        s13
                                                                                        (cons
                                                                                         s14
                                                                                         (cons
                                                                                          s15
                                                                                          (cons
                                                                                           s16
                                                                                           (cons
                                                                                            s17
                                                                                            (cons
                                                                                             s18
                                                                                             (cons
                                                                                              s19
                                                                                              (cons
                                                                                               s20
                                                                                               (cons
                                                                                                s21
                                                                                                (as
                                                                                                 nil
                                                                                                 (ListX State_assert_v_291)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       (((mkTuple2 oks ss) (not (andReduce oks)))))))
(check-sat)
(pop)