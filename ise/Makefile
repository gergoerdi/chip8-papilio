MAIN		= Video
TOP		= Video
VHDL_MODS	=
XAW_MODS	= dcm_32_to_50p35
HS_MODS		= Utils DCM Video

LAVADIR		= ../src
HS_SRCS		= $(patsubst %, $(LAVADIR)/%.hs, $(HS_MODS) $(MAIN))
SRCDIR		= $(LAVADIR)/vhdl
GENSRCDIR	= gensrc
FPGA		= XC3S500E-VQ100-5
XSTDIR		= xst

UCF		= $(patsubst %, $(GENSRCDIR)/%.ucf, $(MAIN))
LAVA_VHDLS	= $(patsubst %, $(GENSRCDIR)/%.vhdl, lava-prelude $(MAIN))
XAW_VHDLS	= $(patsubst %, $(GENSRCDIR)/%.vhdl, $(XAW_MODS))
VHDL_SRCS	= $(patsubst %, $(SRCDIR)/%.vhdl, $(VHDL_MODS))
XAW_SRCS	= $(patsubst %, ipcore_dir/%.xaw, $(XAW_MODS))
VHDLS		= $(LAVA_VHDLS) $(XAW_VHDLS) $(VHDL_SRCS)

GEN_EXTS	= bgn bit bld drc ncd ngc ngd ngr pad par pcf ptwx syr unroutes ut xpi xst prj
GEN_MAP_EXTS	= map mrp ncd ngm
GENS		= $(LAVA_VHDLS) $(UCF) \
		  $(patsubst %, $(MAIN).%, $(GEN_EXTS)) \
		  $(patsubst %, $(MAIN)_map.%, $(GEN_MAP_EXTS)) \
		  $(patsubst %, $(MAIN)_%, bitgen.xwbt ngdbuild.xrpt pad.csv pad.txt summary.xml usage.xml xst.xrpt) \
		  $(XAW_VHDLS) \
		  $(patsubst %, $(GENSRCDIR)/%_arwz.ucf, $(XAW_MODS)) \
		  $(TOP).lso $(TOP)_map.xrpt $(TOP)_par.xrpt \
		  $(TOP)_summary.html $(TOP)_envsettings.html \
		  usage_statistics_webtalk.html webtalk.log xaw2vhdl.log webtalk_pn.xml \
		  arwz.log

XILINX_ROOT	= $(HOME)/prog/fpga/Xilinx/14.2/ISE_DS/ISE/bin/lin64
PAPILIO_PROG	= $(HOME)/bin/papilio-prog
RUNGHC		= runghc

all: $(MAIN).bit

clean:
	rm -f $(GENS)
	rm -rf _xmsgs ngo $(wildcard xlnx_auto_*_xdb) xst

$(MAIN).xise: Makefile xise.in
	sed xise.in -e 's%@@FILES@@%\
		$(foreach FILENAME, $(VHDL_SRCS) $(LAVA_VHDLS), <file xil_pn:name="$(FILENAME)" xil_pn:type="FILE_VHDL">\n<association xil_pn:name="BehavioralSimulation" />\n<association xil_pn:name="Implementation" />\n</file>\n) \
		$(foreach FILENAME, $(UCF), <file xil_pn:name="$(FILENAME)" xil_pn:type="FILE_UCF">\n<association xil_pn:name="Implementation" />\n</file>\n) \
		$(foreach FILENAME, $(XAW_SRCS), <file xil_pn:name="$(FILENAME)" xil_pn:type="FILE_XAW">\n<association xil_pn:name="BehavioralSimulation" />\n<association xil_pn:name="Implementation" />\n</file>\n)%' > $@


$(LAVA_VHDLS): $(HS_SRCS)
	$(RUNGHC) -i$(LAVADIR) $(LAVADIR)/$(MAIN).hs

$(MAIN).prj: Makefile
	echo $(foreach VHDL_SRC,$(VHDLS),'vhdl work "$(VHDL_SRC)"\n') > $@

$(MAIN).xst: Makefile xst.in
	sed -e 's/@MAIN@/$(MAIN)/g;s/@TOP@/$(TOP)/g;' < xst.in > $@

gensrc/%.vhdl: ipcore_dir/%.xaw
	$(XILINX_ROOT)/xaw2vhdl $< -st XST $@

$(MAIN).ut: Makefile ut.in
	sed -e '' < ut.in > $@

$(MAIN).ngc: $(MAIN).prj $(MAIN).xst $(VHDLS)
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/xst -ifn $(MAIN).xst -ofn $(MAIN).syr

$(MAIN).ngd: $(MAIN).ngc $(UCF)
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/ngdbuild -dd ngo -nt timestamp -uc $(UCF) -p $(FPGA) $(MAIN).ngc $(MAIN).ngd

$(MAIN)_map.ncd $(MAIN).pcf: $(MAIN).ngc $(MAIN).ngd
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/map -p $(FPGA) -cm area -ir off -pr off -c 100 -o $(MAIN)_map.ncd $(MAIN).ngd $(MAIN).pcf

$(MAIN).ncd: $(MAIN)_map.ncd $(MAIN).pcf
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/par -w -ol high -t 1 $(MAIN)_map.ncd $(MAIN).ncd $(MAIN).pcf

$(MAIN).bit: $(MAIN).ut $(MAIN).ncd
	mkdir -p $(XSTDIR)/projnav.tmp
	$(XILINX_ROOT)/bitgen -f $(MAIN).ut $(MAIN).ncd

install: $(MAIN).bit
	$(PAPILIO_PROG) -f $(MAIN).bit
