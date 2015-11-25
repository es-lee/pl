Exercise 1 "점프마저 설탕"
======

- 뼈대코드 : http://ropas.snu.ac.kr/~ta/4190.310/15/document/Exn_skel.zip
- 뼈대코드에는 xexp의 언어정의, 파서 및 실행기가 제공됩니다. 뼈대코드 디렉토리에서 make 명령을 실행하시면 숙제 전체가 컴파일되고, 'run' 이라는 실행파일이 생성됩니다.
run 파일을 다음과 같이 실행하시면 입력으로 받은 xexp 프로그램을 여러분이 작성한 removeExn 함수를 사용해서 변환한 다음 실행합니다.
변환된 프로그램에 raise/handle 설탕 문법구조가 남아있다면 에러 메시지도 같이 출력해 줍니다.

    > $ make
    > $ ./run examples/test1.xexp

- 그 외에, -pdesug 옵션을 주어, 변환된(desugared) 프로그램을 출력해 볼 수도 있습니다. 자세한 것은 README 파일을 참고하시기 바랍니다.

- 뼈대코드 중 desuagr.ml 의 removeExn 함수를 구현한 다음, desugar.ml 파일을 제출합니다. 꼭 주의해 주세요, 엉뚱한 ml 파일을 잘못 제출하시면 조교팀이 해결해 드릴 방법이 없습니다.

- Equal(e1, e2) 과 App (e1, e2) 에서 계산 순서는 e1이 먼저인 것으로 정합니다. 따라서 e1에서 예외가 발생하면 e2는 예외가 발생하든 에러(타입 에러, unbound 변수 등)가 발생하든 무시되어야 합니다.
Handle(e1, n, e2)의 계산 순서는 hw7.pdf 문서에 자세히 나와있습니다.
