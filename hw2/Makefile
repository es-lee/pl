all: 1 2 3 4 5 6 7 clean

1: commonGrade.cmo hw2_1.cmo
	ocaml commonGrade.cmo hw2_1.cmo hw2_1_grade.ml

2: commonGrade.cmo hw2_2.cmo
	ocaml commonGrade.cmo hw2_2.cmo hw2_2_grade.ml

3: commonGrade.cmo hw2_3.cmo
	ocaml commonGrade.cmo hw2_3.cmo hw2_3_grade.ml

4: commonGrade.cmo hw2_4.cmo
	ocaml commonGrade.cmo hw2_4.cmo hw2_4_grade.ml

5: commonGrade.cmo hw2_5.cmo
	ocaml commonGrade.cmo hw2_5.cmo hw2_5_grade.ml

6: commonGrade.cmo hw2_6.cmo
	ocaml commonGrade.cmo hw2_6.cmo hw2_6_grade.ml

7: commonGrade.cmo hw2_7.cmo
	ocaml commonGrade.cmo hw2_7.cmo hw2_7_grade.ml

commonGrade.cmo:
	ocamlc commonGrade.ml

hw2_1.cmo: hw2_1.ml
	ocamlc hw2_1.ml

hw2_2.cmo: hw2_2.ml
	ocamlc hw2_2.ml

hw2_3.cmo: hw2_3.ml
	ocamlc hw2_3.ml

hw2_4.cmo: hw2_4.ml
	ocamlc hw2_4.ml

hw2_5.cmo: hw2_5.ml
	ocamlc hw2_5.ml

hw2_6.cmo: hw2_6.ml
	ocamlc hw2_6.ml

hw2_7.cmo: hw2_7.ml
	ocamlc hw2_7.ml

clean:
	rm -fr a.out *.cmo *.cmi
