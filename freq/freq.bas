10    '  START                                        '�C�L�W�פ����ϱM�ε{��
20    KEY OFF: MO=-1: PR=0: D$="A:\"                  'FILENAME: FREQ.BAS
100   '  MENU                                         '�ϥλy��: BASIC
110   GOSUB 4000: F1=0: MF=0: MX=0: NX=99999!: FF=NX: MY=.5
115   IF ST THEN ERASE P,N,M,S,S$,A
120   LOCATE  7,25: PRINT "1. ��  ��  �}  ��"
130   LOCATE  9,25: PRINT "2. �O�_�C�L�W�פ�����"
140   LOCATE 11,25: PRINT "3. �] �w �� �� ��"
150   LOCATE 13,25: PRINT "4. ��  ��  ��  �J"
160   LOCATE 15,25: PRINT "5. ��  �L  ��  �J"
170   LOCATE 17,25: PRINT "6. ��         ��"
180   LOCATE 19,25: PRINT "�� �@ �� ��? (1-6)"
190   GOSUB 700: A=VAL(E$): ST=1
200   IF A<1 OR A>6 THEN 180
210   ON A GOTO 300, 500, 600, 1000, 2000, 5000
300   '   ��  ��  �}  ��
310   MO=NOT MO: GOSUB 4020: GOTO 180
500   '   �O�_�C�L�W�פ�����
510   PR=NOT PR: GOSUB 4020: GOTO 180
600   '   �] �w �� �� ��
610   LOCATE 22,1: INPUT "����ɦs���Ϻо�? (A��E  �۩w�� A:\�Ϻо�) ",D$
620   IF D$="" THEN D$="A:\"
630   IF LEN(D$)=1 THEN D$=D$+":\"
640   LOCATE 22,1: PRINT SPACE$(79)
650   GOSUB 4020: GOTO 180
700   '   GET E$
710   E$=INPUT$(1)
720   IF E$="n" THEN E$="N"
730   IF E$="y" THEN E$="Y"
740   RETURN
800   '  �D�����B���
810   FOR J = 1 TO N(I)
820      P(I,J)=P(I,J)/A(I): IF P(I,J)>.5 THEN MY=1
830   NEXT J: IF MO THEN 880
840   FOR J = 1 TO 2
850     IF MF<M(J,I)+S(J,I) THEN MF=M(J,I)+S(J,I)
860     IF FF>M(J,I)-S(J,I) THEN FF=M(J,I)-S(J,I)
870   NEXT J
880   RETURN
1000  ' �ۺϺо�Ū���
1010  GOSUB 4000: FILES D$
1020  PRINT "�п�J����ɦW��:   ( �Y�L���ɽЧ󴫺Ϥ���� ENTER )": PRINT
1030  INPUT "=> ",NN$: IF NN$="" THEN 1000
1040  IF LEN(NN$)<>3 THEN N$=NN$: GOSUB 1500: GOTO 100
1100  '  CHAIN FILES
1110  FOR I = 1 TO 4
1120    A$=MID$(NN$,I,1)
1130    IF A$>"Z" THEN A$=CHR$(ASC(A$)-32)
1140    B$=B$+A$
1150  NEXT
1160  IF B$<>"ALL" THEN N$=NN$: GOSUB 1500: GOTO 100
1170  OPEN D$+"ALL" FOR INPUT AS #2: F1=1
1180  IF EOF(2) THEN 1210
1190  INPUT #2,N$: GOSUB 4000: PRINT "## LINK: ";N$: GOSUB 1500
1200  ERASE P,N,M,S,S$,A: GOTO 1180
1210  CLOSE #2: GOSUB 4000: PRINT " END  LINK!! ": GOSUB 700: GOTO 100
1500  '  Now reading!
1510  OPEN D$+N$ FOR INPUT AS #1: INPUT #1,Q: INPUT #1,S
1520  DIM P(Q,50),N(Q),M(2,Q),S(2,Q),S$(Q),A(Q)
1530  FOR I = 1 TO Q: INPUT #1,S$(I): INPUT #1,N(I)
1540    IF MO THEN 1570
1550    INPUT #1,M(1,I),SS: S(1,I)=SQR(SS)
1560    INPUT #1,M(2,I),SS: S(2,I)=SQR(SS)
1570    INPUT #1,F: X=F+(N(I)-1)*S: P(I,0)=F: IF MX<X THEN MX=X
1580    IF NX>F THEN NX=F
1590    FOR J = 1 TO N(I)
1600      INPUT #1,F: P(I,J)=F: A(I)=A(I)+F
1610    NEXT J: GOSUB 800
1620  NEXT I: CLOSE #1
1630  GOSUB 3000: RETURN
2000  ' ����L��J���
2010  GOSUB 4000: PRINT "�ҿ�J����ƬO�_�n�s�J�ϺФ�? (Y/N)"
2020  GOSUB 700: IF E$="Y" THEN SA=1: GOTO 2040
2030  IF E$="N" THEN 2100 ELSE 2020
2040  FILES D$: PRINT "      ( �`�N! �Y�P�Ϥ����w�����ɮצP�W�A�h�즳������ɷ|�Q�\��!) "
2050  PRINT: PRINT "  �п�J����ɦW��: "
2060  INPUT "=> ",NN$: PRINT: IF LEN(NN$) THEN 2100
2070  BEEP: PRINT "�A�S����J����ɦW!!": GOSUB 700: GOTO 2000
2100  '   KEY IN DATA
2105  PRINT "�n! �{�b�A�Фp�߿�J��ơA�����N�·ФF...": PRINT
2110  INPUT "���X�i�ϭn�P�ɱƦC���? ",Q: PRINT
2120  INPUT "�նZ? (�Y���u�@�i�ϡA�п�J�@�P���նZ) ",S
2130  DIM P(Q,50),N(Q),M(2,Q),S(2,Q),S$(Q),A(Q)
2140  FOR I = 1 TO Q
2150    GOSUB 4000: PRINT "��";I;"�i��"
2160    INPUT "���D: ",S$(I)
2170    INPUT "�ռ�: ",N(I)
2180    IF MO THEN 2210
2190    INPUT "�����������B�ܤ�: (Mean,SS)",M(1,I),SS: S(1,I)=SQR(SS)
2200    INPUT "�����������B�ܤ�: (Mean,SS)",M(2,I),SS: S(2,I)=SQR(SS)
2210    INPUT "���: (�Y���Ϥ����䭭) ",F: X=F+(N(I)-1)*S: P(I,0)=F
2220    IF MX<X THEN MX=X
2230    IF NX>F THEN NX=F
2240    FOR J = 1 TO N(I)
2250      PRINT J;". ";P(I,0)+(J-1)*S;"��";P(I,0)+J*S;": ";
2260      INPUT "",F: P(I,J)=F: A(I)=A(I)+F
2270    NEXT J: GOSUB 800
2280  NEXT I: IF SA=0 THEN 2610
2500  ' Now saving data into disk!
2510  OPEN D$+NN$ FOR OUTPUT AS #1
2520  PRINT #1,Q: PRINT #1,S
2530  FOR I = 1 TO Q
2540    PRINT #1,S$(I): PRINT #1,N(I)
2550    IF MO THEN 2580
2560    PRINT #1,M(1,I);",";S(1,I)^2
2570    PRINT #1,M(2,I);",";S(2,I)^2
2580    PRINT #1,P(I,0)
2590    FOR J = 1 TO N(I): PRINT #1,P(I,J): NEXT J
2600  NEXT I: CLOSE #1
2610  GOSUB 3000: GOTO 100
3000  '  ø�s�W�פ�����
3010  IF MF<MX THEN MF=MX
3020  IF FF>NX THEN FF=NX
3030  FOR I = 1 TO Q: CLS: PRINT S$(I)
3100  ' �e�y�жb
3110    WINDOW (2*FF-2*S-MF,-MY*.2)-(MF,MY*1.3): LOCATE 3,1: W=MF-FF+S
3120    IF MO THEN 3170
3130    PRINT "P1:":PRINT USING "    Mean =###.####";M(1,I)
3140    PRINT USING "      S  =###.####";S(1,I): PRINT
3150    PRINT "P2:":PRINT USING "    Mean =###.####";M(2,I)
3160    PRINT USING "      S  =###.####";S(2,I): PRINT
3170    PRINT "Range:": PRINT: PRINT USING "X axis =###.## -###.##";NX,MX+S
3180    PRINT: PRINT USING "Y axis = 0 - ###%";MY*100
3190    LINE (NX-S,MY)-(NX-S,0): LINE -(MX,0)
3200    FOR J = 0 TO MY STEP MY/5
3210    LINE (NX-S,J)-(NX-S-W/40,J): NEXT
3300  ' �e�W��
3310    FOR J = 1 TO N(I): F=P(I,0)+(J-1)*S
3320      LINE (F,0)-(F,-MY/40): LINE (F,0)-(F-S,P(I,J)),,BF
3330    NEXT: H=MY*1.1
3340    IF MO=0 THEN GOSUB 3400
3350    GOSUB 700: PRINT CHR$(27);"H;"
3360  NEXT I
3370  IF PR   THEN GOSUB 3500
3380  RETURN
3400  ' �e SYMBOL
3410  FOR J = 1 TO 2: HH=H+(2-J)*H/13
3420    CIRCLE (M(J,I),HH),W/80,,,,.8
3430    LINE (M(J,I)-S(J,I),HH)-(M(J,I)+S(J,I),HH)
3440    LINE (M(J,I)-S(J,I),HH-MY/40)-(M(J,I)-S(J,I),HH+MY/40)
3450    LINE (M(J,I)+S(J,I),HH-MY/40)-(M(J,I)+S(J,I),HH+MY/40)
3460  NEXT J: RETURN
3500  ' �C�L�W�פ�����
3510  FOR I = 1 TO Q
3520    LPRINT: LPRINT S$(I): LPRINT: IF MO THEN 3570
3530    LPRINT "P1:": LPRINT USING "Mean =###.####";M(1,I)
3540    LPRINT USING "  S  =###.####";S(1,I): LPRINT
3550    LPRINT "P2:": LPRINT USING "Mean =###.####";M(2,I)
3560    LPRINT USING "  S  =###.####";S(2,I): LPRINT
3570    LPRINT " �լ�    �����W��    �۹��W��"
3580    FOR J = 1 TO N(I)
3590      LPRINT USING "-##.## :  ###.##      ###.##%";P(I,0)+J*S,P(I,J)*A(I),P(I,J)*100
3600    NEXT J
3610  NEXT I: RETURN
4000  '  PRINT TITLE
4010  CLS: PRINT CHR$(27)+"~T24,10,10;*    ��* �C�L�W�פ����ϱM�ε{��"+CHR$(27)+"~T16;"
4020  IF MO THEN MO$="�@��" ELSE MO$="�|��"
4030  IF PR THEN PR$="�n"   ELSE PR$="��"
4040  PRINT CHR$(27)+"~T24,22,10;"+MO$+CHR$(27)+"~T16;"
4050  LOCATE 2,55: PRINT USING "DATE: \   \   TIME: \   \";DATE$,TIME$
4060  LOCATE 23,1: PRINT USING "�t�θ�T | �W�פ�����: \\�L  �����: \     \ �{���]�p: �����j�ǹA���Ǩt  �G��N";PR$,D$
4070  LOCATE  4,1: RETURN
5000  '  ��    ��
5010  GOSUB 4000
5020  LOCATE 10,20: PRINT "�����Ҧ��u�@? (Y/N)"
5030  GOSUB 700: IF E$<>"Y" THEN 100
5040  FOR I = 1 TO 23
5050    PRINT " �� �� �z �� �� ��, �� �� �P ��!        �����A��  �G �� �N  �� �`      79,11,17"
5060  NEXT
5070  FOR I = 1 TO 22: PRINT : NEXT
5080  LOCATE 2,1: SYSTEM
