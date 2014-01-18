      SUBROUTINE BEGIN(IOCODE, NROCK,NTER,NPVT,NTEP,NW,
     &  NTMAX,NRST,N1DIR,N23DIR,NITER,NTRI,
     &  IREOPT,IPLOTP,IRNUM,IRSTRT,NN,TMAX, RESIN,RESOUT, REDATE)

      INCLUDE 'PARAMS.FOR'

ccc ---> Added the following stm't per Ted Bourgoyne (LSU) - 03/05/95
      CHARACTER*12 RESIN,RESOUT

      DIMENSION REDATE(LP17)

C=======================================================================

      WRITE(IOCODE,3005)

3005  FORMAT(//T10,69('*'),
     & /T10,'*',                                                T78,'*',
     & /T10,'*',T25,'          B O A S T  3           ',        T78,'*',
     & /T10,'*',T25,'  BLACK OIL APPLIED SIMULATION TOOL',      T78,'*',
     & /T10,'*',T25,'Developed for BDM Federal - May, 1992',    T78,'*',
     & /T10,'*',T25,'   Modified for LSU - July, 1993',         T78,'*',
     & /T10,'*',T25,'   Modified for LSU - May, 1995',          T78,'*',
     & /T10,'*',T25,'                                   ',      T78,'*',
     & /T10,69('*') /)

C########################################################################


C      WRITE DIMENSION INFO
      WRITE(IOCODE,3100) LP1,LP2,LP3,NROCK,NTER,NPVT,NTEP,NW,
     & NTMAX,NRST,N1DIR,N23DIR,NITER,NTRI,N23DIR
 3100 FORMAT(T17,'REDIMENSIONING INFORMATION:',
     & /T2,'Max x-direction grid blocks',T62,I5,
     & /T2,'Max y-direction grid blocks',T62,I5,
     & /T2,'Max z-direction grid blocks',T62,I5,
     & /T2,'Max rock regions',T62,I5,
     & /T2,'Max rock region table entries',T62,I5,
     & /T2,'Max pvt regions',T62,I5,
     & /T2,'Max pvt region table entries',T62,I5,
     & /T2,'Max wells',T62,I5,
     & /T2,'Max time steps',T62,I5,
     & /T2,'Max restart records',T62,I5,
     & /T2,'Total blocks using 1D direct soln methods',T62,I5,
     & /T2,'Total blocks using 2D or 3D direct soln methods',T62,I5,
     & /T2,'Total blocks using iterative soln method',T62,I5,
     & /T2,'Max no of blocks in 1D for LSOR',T62,I5,
     & /T2,'Max no of blocks in 2D for L2SOR',T62,I5//)


C** RESTART OPTION READ
      READ(20,69)
      READ(20,*) IREOPT,IPLOTP
69    FORMAT(A75)

C****************************************************************************
C     NOTE:  IPLOTP=1 no longer allowed - SUBROUTINE PLOTI has been removed!
C     -----> IPLOTP=0 now only gives array output! 
      IF(IPLOTP.EQ.1) WRITE(*,701)
      IF(IPLOTP.EQ.1) WRITE(9,701)
      IF(IPLOTP.EQ.1) STOP
701   FORMAT(//,'! IPOSTP must be zero - line printer plots no longer',
     &'  available !')

C     NOTE:  IREOPT= 0 or 1  will work like BOASTII - added back 05/31/95 
C            after converting to MS PowerStation Fortran    

C+++++++++++++ ADDED ALL THIS FROM OLD DISKETTE - 05/31/95 +++++++++++++++++
      IF(IREOPT.GT.-1) READ(20,*) IRNUM,IRSTRT,NN,TMAX
      IF(IREOPT.EQ.-1) WRITE(IOCODE,12)
      IF(IREOPT.EQ.0) WRITE(IOCODE,13)
      IF(IREOPT.EQ.1) WRITE(IOCODE,14) IRSTRT
      IF(IREOPT.GT.-1)
     & WRITE(IOCODE,11) IREOPT,IRNUM,IRSTRT,NN,TMAX
   11 FORMAT(/30X,'RESTART CODES:',
     & /5X,'RESTART OPTION (IREOPT)  ',T70,I5,
     & /5X,'NUMBER OF RESTART ENTRIES (IRNUM)  ',T70,I5,
     & /5X,'RESTART TIME STEP (IRSTRT)  ',T70,I5,
     & /5X,'MAXIMUM NUMBER OF TIME STEPS (NN)  ',T70,I5,
     & /5X,'MAXIMUM NUMBER OF DAYS (TMAX)',T65,F10.1/)
   12 FORMAT(/T15,' RESTART OPTION: ',
     & //T5,'Restart option has not been activated.',//)
   13 FORMAT(/T15,' RESTART OPTION: ',
     & //T5,'Restart option has been activated.',//)
   14 FORMAT(/T15,' RESTART OPTION: ',
     & //T5,'This is a restart run beginning at time step',I5,//)

      IF(IREOPT.LT.0) GO TO 18

      READ(20,7) RESIN
      READ(20,7) RESOUT

C=========================== ADDED 9/30/91 ============================
7     FORMAT(A12)
C======================================================================
      WRITE(IOCODE,3003) RESIN,RESOUT
 3003 FORMAT(/5X,'INPUT RESTART FILE    ',T50,A12,
     & /,5X,'INPUT RESTART FILE    ',T50,A12,/)


C---- Machine dependent open statements for binary restart files ------

      OPEN(UNIT=24,FORM='unformatted',ACCESS='SEQUENTIAL',
     & FILE=RESIN,STATUS='UNKNOWN')
      OPEN(UNIT=25,FORM='unformatted',ACCESS='SEQUENTIAL',
     & FILE=RESOUT,STATUS='UNKNOWN')
      READ(20,*)       (REDATE(I),I=1,IRNUM)
      WRITE(IOCODE,16) (REDATE(I),I=1,IRNUM)
   16 FORMAT(25X,'RESTART TIMES (DAYS):',5(1X,F10.2))
      WRITE(IOCODE,33)
  33  FORMAT(//)
   18 CONTINUE
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C***************************************************************************

      RETURN
      END 
