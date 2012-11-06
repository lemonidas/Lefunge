GL_INCLUDE=lablgl.cma lablglut.cma
GL_FLAGS=-I +lablGL $(GL_INCLUDE)
GL_FILES=GraphicalDebugger/visual_lefunge.ml
GLCC=ocamlc $(GL_FLAGS)
GL_OUT=gl_lefunge

LEF_FILES=Interpreter/lefunge.ml
OCAMLC_FLAGS=
OCAMLC=ocamlc $(OCAMLC_FLAGS)
OUT=lefunge

default : $(OUT)

debug : $(GL_OUT)
gl : $(GL_OUT)

all : $(OUT) $(GL_OUT)

$(GL_OUT) : $(GL_FILES)
	$(GLCC) -o $@ $^

$(OUT) : $(LEF_FILES)
	$(OCAMLC) -o $@ $^

clean: 
	rm *.cm*
