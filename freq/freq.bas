10    '  START                                        '列印頻度分布圖專用程式
20    KEY OFF: MO=-1: PR=0: D$="A:\"                  'FILENAME: FREQ.BAS
100   '  MENU                                         '使用語言: BASIC
110   GOSUB 4000: F1=0: MF=0: MX=0: NX=99999!: FF=NX: MY=.5
115   IF ST THEN ERASE P,N,M,S,S$,A
120   LOCATE  7,25: PRINT "1. 形  式  開  關"
130   LOCATE  9,25: PRINT "2. 是否列印頻度分布表"
140   LOCATE 11,25: PRINT "3. 設 定 磁 碟 機"
150   LOCATE 13,25: PRINT "4. 檔  案  輸  入"
160   LOCATE 15,25: PRINT "5. 鍵  盤  輸  入"
170   LOCATE 17,25: PRINT "6. 結         束"
180   LOCATE 19,25: PRINT "操 作 項 目? (1-6)"
190   GOSUB 700: A=VAL(E$): ST=1
200   IF A<1 OR A>6 THEN 180
210   ON A GOTO 300, 500, 600, 1000, 2000, 5000
300   '   形  式  開  關
310   MO=NOT MO: GOSUB 4020: GOTO 180
500   '   是否列印頻度分布表
510   PR=NOT PR: GOSUB 4020: GOTO 180
600   '   設 定 磁 碟 機
610   LOCATE 22,1: INPUT "資料檔存於何磁碟機? (A～E  自定為 A:\磁碟機) ",D$
620   IF D$="" THEN D$="A:\"
630   IF LEN(D$)=1 THEN D$=D$+":\"
640   LOCATE 22,1: PRINT SPACE$(79)
650   GOSUB 4020: GOTO 180
700   '   GET E$
710   E$=INPUT$(1)
720   IF E$="n" THEN E$="N"
730   IF E$="y" THEN E$="Y"
740   RETURN
800   '  求極限、比例
810   FOR J = 1 TO N(I)
820      P(I,J)=P(I,J)/A(I): IF P(I,J)>.5 THEN MY=1
830   NEXT J: IF MO THEN 880
840   FOR J = 1 TO 2
850     IF MF<M(J,I)+S(J,I) THEN MF=M(J,I)+S(J,I)
860     IF FF>M(J,I)-S(J,I) THEN FF=M(J,I)-S(J,I)
870   NEXT J
880   RETURN
1000  ' 自磁碟機讀資料
1010  GOSUB 4000: FILES D$
1020  PRINT "請輸入資料檔名稱:   ( 若無該檔請更換磁片後按 ENTER )": PRINT
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
2000  ' 自鍵盤輸入資料
2010  GOSUB 4000: PRINT "所輸入之資料是否要存入磁碟中? (Y/N)"
2020  GOSUB 700: IF E$="Y" THEN SA=1: GOTO 2040
2030  IF E$="N" THEN 2100 ELSE 2020
2040  FILES D$: PRINT "      ( 注意! 若與磁片中已有之檔案同名，則原有之資料檔會被蓋掉!) "
2050  PRINT: PRINT "  請輸入資料檔名稱: "
2060  INPUT "=> ",NN$: PRINT: IF LEN(NN$) THEN 2100
2070  BEEP: PRINT "你沒有輸入資料檔名!!": GOSUB 700: GOTO 2000
2100  '   KEY IN DATA
2105  PRINT "好! 現在，請小心輸入資料，打錯就麻煩了...": PRINT
2110  INPUT "有幾張圖要同時排列比較? ",Q: PRINT
2120  INPUT "組距? (若不只一張圖，請輸入共同之組距) ",S
2130  DIM P(Q,50),N(Q),M(2,Q),S(2,Q),S$(Q),A(Q)
2140  FOR I = 1 TO Q
2150    GOSUB 4000: PRINT "第";I;"張圖"
2160    INPUT "標題: ",S$(I)
2170    INPUT "組數: ",N(I)
2180    IF MO THEN 2210
2190    INPUT "父本之平均、變方: (Mean,SS)",M(1,I),SS: S(1,I)=SQR(SS)
2200    INPUT "母本之平均、變方: (Mean,SS)",M(2,I),SS: S(2,I)=SQR(SS)
2210    INPUT "初值: (即全圖之左邊限) ",F: X=F+(N(I)-1)*S: P(I,0)=F
2220    IF MX<X THEN MX=X
2230    IF NX>F THEN NX=F
2240    FOR J = 1 TO N(I)
2250      PRINT J;". ";P(I,0)+(J-1)*S;"～";P(I,0)+J*S;": ";
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
3000  '  繪製頻度分布圖
3010  IF MF<MX THEN MF=MX
3020  IF FF>NX THEN FF=NX
3030  FOR I = 1 TO Q: CLS: PRINT S$(I)
3100  ' 畫座標軸
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
3300  ' 畫柱形
3310    FOR J = 1 TO N(I): F=P(I,0)+(J-1)*S
3320      LINE (F,0)-(F,-MY/40): LINE (F,0)-(F-S,P(I,J)),,BF
3330    NEXT: H=MY*1.1
3340    IF MO=0 THEN GOSUB 3400
3350    GOSUB 700: PRINT CHR$(27);"H;"
3360  NEXT I
3370  IF PR   THEN GOSUB 3500
3380  RETURN
3400  ' 畫 SYMBOL
3410  FOR J = 1 TO 2: HH=H+(2-J)*H/13
3420    CIRCLE (M(J,I),HH),W/80,,,,.8
3430    LINE (M(J,I)-S(J,I),HH)-(M(J,I)+S(J,I),HH)
3440    LINE (M(J,I)-S(J,I),HH-MY/40)-(M(J,I)-S(J,I),HH+MY/40)
3450    LINE (M(J,I)+S(J,I),HH-MY/40)-(M(J,I)+S(J,I),HH+MY/40)
3460  NEXT J: RETURN
3500  ' 列印頻度分布表
3510  FOR I = 1 TO Q
3520    LPRINT: LPRINT S$(I): LPRINT: IF MO THEN 3570
3530    LPRINT "P1:": LPRINT USING "Mean =###.####";M(1,I)
3540    LPRINT USING "  S  =###.####";S(1,I): LPRINT
3550    LPRINT "P2:": LPRINT USING "Mean =###.####";M(2,I)
3560    LPRINT USING "  S  =###.####";S(2,I): LPRINT
3570    LPRINT " 組界    絕對頻度    相對頻度"
3580    FOR J = 1 TO N(I)
3590      LPRINT USING "-##.## :  ###.##      ###.##%";P(I,0)+J*S,P(I,J)*A(I),P(I,J)*100
3600    NEXT J
3610  NEXT I: RETURN
4000  '  PRINT TITLE
4010  CLS: PRINT CHR$(27)+"~T24,10,10;*    用* 列印頻度分布圖專用程式"+CHR$(27)+"~T16;"
4020  IF MO THEN MO$="一般" ELSE MO$="育種"
4030  IF PR THEN PR$="要"   ELSE PR$="不"
4040  PRINT CHR$(27)+"~T24,22,10;"+MO$+CHR$(27)+"~T16;"
4050  LOCATE 2,55: PRINT USING "DATE: \   \   TIME: \   \";DATE$,TIME$
4060  LOCATE 23,1: PRINT USING "系統資訊 | 頻度分布表: \\印  資料檔: \     \ 程式設計: 中興大學農藝學系  鄭文吉";PR$,D$
4070  LOCATE  4,1: RETURN
5000  '  結    束
5010  GOSUB 4000
5020  LOCATE 10,20: PRINT "結束所有工作? (Y/N)"
5030  GOSUB 700: IF E$<>"Y" THEN 100
5040  FOR I = 1 TO 23
5050    PRINT " 謝 謝 您 的 使 用, 謹 此 致 謝!        中興農藝  鄭 文 吉  鞠 躬      79,11,17"
5060  NEXT
5070  FOR I = 1 TO 22: PRINT : NEXT
5080  LOCATE 2,1: SYSTEM
