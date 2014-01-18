C     Parameters for BOAST3 - 08/11/96 - could not find latest file PARAMS.FOR
C                                      - recreated from an old one! 
C.... LP1  =  Max x-dir grid-blocks
C.... LP2  =  Max y-dir grid-blocks
C.... LP3  =  Max z-dir grid-blocks
C.... LP7  =  Max number of rock regions
C.... LP8  =  Max number of PVT regions
C.... LP9  =  Max number of table entries
C.... LP10 =  Max of {LP7,LP8} 
C.... LP11 =  Max number of well-blocks 60
C.... LP12 =  Max number of time-steps
C.... LP15 =  Equal to or greater than Max of {LP1,LP2,LP3} 
C.... LP17 =  Max number of restart records

C.... Specified parameters..............................................
      PARAMETER (LP1=100, LP2=100,  LP3=20,  LP15=500)
cc      PARAMETER (LP1=30, LP2=7,  LP3=28,  LP15=30)

      PARAMETER (LP7=3,  LP8=3,  LP9=25, LP10=3, LP11=60, LP12=5000)
      PARAMETER (LP14=1, LP17=3)
C:::: NOTES:  LP14 must be 1 for BOASTII PC Version & BOAST3  

C.... Derived parameters................................................
      PARAMETER (LP4=LP1+1, LP5=LP2+1, LP6=LP3+1, LP13=LP1+LP2+LP3)
      PARAMETER (LP19=LP4*LP2*LP3, LP20=LP1*LP5*LP3)
      PARAMETER (LP21=LP1*LP2*LP6, LP22=LP1*LP2*LP3, LP23=LP11*LP3)

C.... New parameters for B3PLOT and/or COLORGRID  -  07/02/93
C.... Max number of wells = NWX;  Max nodes per well = NODX

      PARAMETER(NWX=LP11, NODX=LP3)
      PARAMETER(NODWX=NWX*NODX)
      PARAMETER(IMX=LP1, JMX=LP2, KMX=LP3)
