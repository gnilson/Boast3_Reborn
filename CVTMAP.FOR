
C================ COPIED THIS FROM C:\GP2  - 09/21/96 
C================ COULD NOT FIND COPY FOR BOAST3 
C================ THIS SHOULD BE EXACTLY THE SAME IN ALL MODELS 


C**** DATE   :       May 04, 1994     

C**** PROGRAM:       CVTMAP - COLORGRID       
C
C**** FUNCTION:   To read ____.MAP  (binary) file from simulator (MISC4) and
C****             convert to ASCII file for input to PASCAL pgm MAPCVT.PAS

C**** COMMAND FILE:  COLOR.BAT  (!!! This program is designed to be run "only"
C****                            by the Batch Command File Color.Bat !!!)

C**** NOTE:  This pgm all single precision.........


C========================== CONSTRAINTS =====================================
C**** NOTE:  Allows up to 6000 blocks - any combination - so can use with
C****        any simulator - probably forever !!!  

C**** NOTE:  FORMAT 15 will allow 6000 blocks - and probably "never" will
C****        need to be changed!

C**** NOTE:  Up to 9999 records  (including trailer) are permitted here !
C**** NOTE:  But maximum of 230 records permitted in COLORGRD !!!!!
C============================================================================

CCC      INCLUDE 'PARAMS.INC'
CCC      DIMENSION    R4ARY(IJKM)

      CHARACTER*30 PARAM

C.... Will handle up to 6000 grid-blocks (see also FORMAT 15 below) !
      DIMENSION    R4ARY(6000)

c.... Open colorgrd.dat and get record length for ___.map file..........
      OPEN(17, FILE = 'COLORGRD.DAT', STATUS = 'OLD')
      READ(17,*)
      READ(17,*)
      READ(17,*) II,JJ,KK,KRAD,KDY
      IJKMAX = II*JJ*KK

      DO 100 I=4,8
      READ(17,*) 
100   CONTINUE
      READ(17,*) MAPREC 

c.... Open direct access binary file _____.MAP (renamed to T.MAP by COLOR.BAT)
      OPEN(18, FILE = 'T.MAP', STATUS = 'OLD',  FORM= 'UNFORMATTED',
     &         ACCESS='DIRECT', RECL=MAPREC, ERR=9999)
   
c.... Read T.MAP file and convert to ASCII file "COLORMAP.DAT"
      OPEN(19, FILE = 'COLORMAP.DAT', STATUS = 'NEW')

      IREC = 0
      DO 200 L=1,9999
c.... Read record from ___.MAP file.....................................
      IREC = IREC+1
      READ(18,REC=IREC)  R4TIME,PARAM, (R4ARY(K), K=1,IJKMAX)

c.... Write record (formatted) to ColorMap.Dat file.....................
      WRITE(19,15)               R4TIME, PARAM, (R4ARY(K), K=1,IJKMAX)

      IF(L.GE.230) THEN
         WRITE(*,201)
201      FORMAT(/
     &   /1X,'... 230 maps (the maximum number permitted in COLORGRD)',
     &   /1X,'... have been written.  No more maps will be processed.',
     &   /1X,'... You may edit the COLORMAP.DAT file to determine the',
     &   /1X,'... last map processed.'/)
         PAUSE '          ---------- Enter to continue ----------'
         GO TO 302
      ENDIF

200   CONTINUE

  
300   CONTINUE

302   write(19,19)
19    format(1X,' -1.000000                               ')
15    format(1X,F10.4,1X,A30,6000F9.3) 

      CLOSE(17)
      CLOSE(18)
      CLOSE(19)

      STOP ' '

9999  WRITE(*,8887)
8887  FORMAT(//5X,'!!! Error in opening T.MAP file in pgm CVTMAP !!!'//) 
      STOP

      END
