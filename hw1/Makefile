all: 1 2 3 4 5

1: commonGrade.cmo hw1_1.cmo
	ocaml commonGrade.cmo hw1_1.cmo hw1_1_grade.ml

2: commonGrade.cmo hw1_2.cmo
	ocaml commonGrade.cmo hw1_2.cmo hw1_2_grade.ml

3: commonGrade.cmo hw1_3.cmo
	ocaml commonGrade.cmo hw1_3.cmo hw1_3_grade.ml

4: commonGrade.cmo hw1_4.cmo
	ocaml commonGrade.cmo hw1_4.cmo hw1_4_grade.ml

5: commonGrade.cmo hw1_5.cmo
	ocaml commonGrade.cmo hw1_5.cmo hw1_5_grade.ml

commonGrade.cmo:
	ocamlc commonGrade.ml

hw1_1.cmo: hw1_1.ml
	ocamlc hw1_1.ml

hw1_2.cmo: hw1_2.ml
	ocamlc hw1_2.ml

hw1_3.cmo: hw1_3.ml
	ocamlc hw1_3.ml

hw1_4.cmo: hw1_4.ml
	ocamlc hw1_4.ml

hw1_5.cmo: hw1_5.ml
	ocamlc hw1_5.ml

clean:
	rm -fr a.out *.cmo *.cmi
