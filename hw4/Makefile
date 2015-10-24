all: run clean

run: commonGrade.cmo hw4_2.cmo
	ocaml commonGrade.cmo hw4_2.cmo hw4_2_grade.ml

commonGrade.cmo:
	ocamlc commonGrade.ml

hw4_2.cmo: hw4_2.ml
	ocamlc hw4_2.ml

clean:
	rm -fr a.out *.cmo *.cmi
