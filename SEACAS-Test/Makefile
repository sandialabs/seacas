NPROCA = 31
NPROCB = 32
NPROCC = 33

BIN = ../bin
APREPRO	=	$(BIN)/aprepro
FASTQ	=	$(BIN)/fastq
GJOIN	=	$(BIN)/gjoin
GEN3D	=	$(BIN)/gen3d
GENSHELL=	$(BIN)/genshell
GREPOS	=	$(BIN)/grepos
IO_SHELL = 	$(BIN)/io_shell
EXOTXT  =       $(BIN)/exotxt
TXTEXO  =       $(BIN)/txtexo
EPU     =       $(BIN)/epu
EXODIFF =       $(BIN)/exodiff
DECOMP  =       $(BIN)/decomp
EXO2MAT =       $(BIN)/exo2mat
MAT2EXO =       $(BIN)/mat2exo
CONJOIN =       $(BIN)/conjoin
RM	=	rm -f

.SUFFIXES: .g .fsq .g3

.fsq.g:
	$(RM) $@
	$(FASTQ) -aprepro -mesh=$*.g $*.fsq

test:	baseline.g3 new_tape.g3 new_shell.g3 slice conv_text matlab python conjoin

base_o.g3: base_o.g base_o.g3i 
	$(APREPRO) base_o.g3i |	$(GEN3D) base_o.g base_o.g3

base.g3: base.g base.g3i 
	$(APREPRO) base.g3i | $(GEN3D)  base.g base.g3

base_sh.g3: base_sh.g base_sh.g3i 
	$(APREPRO) base_sh.g3i | $(GENSHELL)  base_sh.g base_sh.g3

cap.g3: cap.g cap.g3i 
	$(APREPRO) cap.g3i | $(GEN3D)  cap.g cap.g3

new_tape.g3: base.g3 base_o.g3 rigid.g3 cap.g3 new_tape.gj
	$(GJOIN) <new_tape.gj

new_shell.g3: base_sh.g3 base_o.g3 rigid.g3 cap.g3 new_shell.gj
	$(GJOIN) <new_shell.gj

rigid.g3: rigid.g rigid.g3i
	$(GEN3D) rigid.g rigid.g3 <rigid.g3i

#-----------------------------
# NOTE: Add names to blocks, nsets, ssets
baseline.g: baseline.g3 names.grp
	$(GREPOS) baseline.g3 baseline.g < names.grp

baseline.g3: baseline_o.g3  rigid_0.g3 base.g3 baseline.gj cap.g3
	$(GJOIN) <baseline.gj

baseline_o.g3: baseline_o.g base.g3i
	$(APREPRO) base_o.g3i |	$(GEN3D) baseline_o.g baseline_o.g3

rigid_0.g3: rigid.g3 rigid.grp
	$(APREPRO) rigid.grp | $(GREPOS)  rigid.g3 rigid_0.g3

# NOTE: This conversion should preserve the block, nset, and sset names
#       that are in baseline.g
slice: slicea sliceb slicec

slicea: baseline.g
	$(DECOMP) -p $(NPROCA) --subdir mesh baseline.g
	ls -l mesh/baseline.g.$(NPROCA).*
	$(EPU) --max_open_files 10 -output_extension e -auto mesh/baseline.g.$(NPROCA).00 
	$(EXODIFF) -stat baseline.g baseline.e

sliceb: baseline.g
	$(DECOMP) -p $(NPROCB) --subdir mesh baseline.g
	ls -l mesh/baseline.g.$(NPROCB).*
	$(EPU) --max_open_files 8 -output_extension e -auto mesh/baseline.g.$(NPROCB).00 
	$(EXODIFF) -stat baseline.g baseline.e

slicec: baseline.g
	$(DECOMP) -p $(NPROCC) --subdir mesh baseline.g
	ls -l mesh/baseline.g.$(NPROCC).*
	$(EPU) -output_extension e -auto mesh/baseline.g.$(NPROCC).00 
	$(EXODIFF) -stat baseline.g baseline.e

# NOTE: This conversion will lose the block, nset, and sset names
#       that are in baseline.g
conv_text: baseline.g
	$(EXOTXT) baseline.g baseline.t
	$(TXTEXO) baseline.t baseline.gt
	$(EXODIFF) -stat baseline.g baseline.gt

matlab: baseline.g
	$(EXO2MAT) baseline.g
	$(MAT2EXO) baseline.mat
	$(EXODIFF) -stat baseline.g baseline.exo

python: baseline.g
	PYTHONPATH=../lib:$$PYTHONPATH python testExo.py
	$(EXODIFF) -stat -pedantic baseline.g baseline_numpy_copy.e
	$(EXODIFF) -stat -pedantic baseline.g baseline_ctype_copy.e
	$(EXODIFF) -stat -pedantic baseline.g baseline_copy.e

conjoin:
	$(IO_SHELL) --in_type generated --split_times 1 10x10x10+times:2000+variables:nodal,3,element,6 conjoin/output.g
	$(CONJOIN) --element_status NONE --nodal_status NONE conjoin/output.g_*
	$(IO_SHELL) --in_type generated 10x10x10+times:2000+variables:nodal,3,element,6 gold.g
	$(EXODIFF) -stat -pedantic conjoin-out.e gold.g

############################# Default Rules#####################
# Dependencies:
base_o.g: common.h
base.g: common.h

clean:
	$(RM) *.g3 *.g *.e *.gt *.t *~ core*
	$(RM) -r mesh/ tmp.g3d
	$(RM) *.decomp.out *.nem *.pex
	$(RM) *.mat *.exo
	$(RM) baseline_*_copy.e baseline_copy.e
	$(RM) -r conjoin
