PROGRAM TSFOIL

!     PROGRAM TSFOIL(INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT,TAPE3,TAPE7)

!***********************************************************************
!
!                  MAIN PROGRAM FOR TSFOIL
!                  PROGRAM COMPUTES TRANSONIC FLOW PAST A TWODIMENSIONAL
!                  LIFTING AIRFOIL USING TRANSONIC SMALL DISTURBANCE THE
!
!                  PROGRAM WRITTEN BY
!                       EARLL M. MURMAN AND FRANK R. BAILEY
!                       NASA-AMES RESEARCH CENTER
!                            AND
!                       MARGARET L. JOHNSON
!                       COMPUTER SCIENCES CORPORATION
!                  version for VT Aerospace Design Space software
!
!                  REFERENCES FOR PROGRAM USAGE ARE
!                  (  TO BE FILLED IN LATER)
!
!                  from Joh, VPI, with modifications
!                                 by w.h. mason for mac operation
!                                 and with diamond airfoil option
!
!                  mod to I/O to fix bug, April 2, 1998
!                  mod to output file for plotting, April 3, 2000
!                  mod to run on PC
!					- Added directory selection
!					- Added Pause statement at end of program
!					- Added more comments in the header
!                   - Program outputs to a file now
!				   Andy Ko 4/2/03.
!                  Fixed bug in Jameson input section 4/9/03 - Andy Ko
!
!***********************************************************************

    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON / COM5/ XDIFF(100),YDIFF(100)
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
    COMMON / COM8/ CVERGE   , DVERGE  , IPRTER  , MAXIT   ,&
    &WE(3)    , EPS
    INTEGER        BCFOIL
    COMMON / COM9/ BCFOIL   , NL      , NU      , XL(100) , XU(100) ,&
    &YL(100)  , YU(100) , RIGF,IFLAP,DELFLP,FLPLOC
    COMMON /COM10/ YFREE(100),YTUN(100),GAM     , JMXF    , JMXT
    INTEGER        PSTART
    LOGICAL        PSAVE
    COMMON /COM11/ ALPHAO   , CLOLD   , DELTAO  , DUBO    , EMACHO  ,&
    &IMINO    , IMAXO   , IMAXI   , JMINO   , JMAXO   ,&
    &JMAXI    , PSAVE   , PSTART  ,TITLE(20),TITLEO(20),&
    &VOLO     , XOLD(100),YOLD(100)
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        FCR      , KUTTA
    COMMON /COM14/ CLSET    , FCR     , KUTTA   , WCIRC
    COMMON /COM15/ B        , BETA0   , BETA1   , BETA2   , PSI0    ,&
    &PSI1     , PSI2
    REAL JET
    COMMON /COM16/ ALPHA0   , ALPHA1  , ALPHA2  , XSING   ,&
    &OMEGA0   , OMEGA1  , OMEGA2  , JET
    COMMON /COM17/ CYYBLC   , CYYBLD  , CYYBLU  , CYYBUC  , CYYBUD  ,&
    &CYYBUU   ,FXLBC(100),FXUBC(100), ITEMP1, ITEMP2
    LOGICAL        OUTERR
    COMMON /COM18/ ERROR    , I1      , I2      , IERROR  , JERROR  ,&
    &OUTERR   , EMU(100,2)        , VC(100) ,&
    &WI       , DCIRC   , POLD(100,2)
    COMMON /COM19/ DIAG(100), RHS(100), SUB(100), SUP(100)
    COMMON /COM20/ XMID(100), YMID(100)
    COMMON /COM21/ CYYLC    , CYYLD   , CYYLU   , CYYUC   , CYYUD   ,&
    &CYYUU
    COMMON /COM22/ CXC(100) , CXL(100), CXR(100), CXXC(100),CXXL(100),&
    &CXXR(100), C1(100)
    COMMON /COM23/ CYYC(100), CYYD(100),CYYU(100), IVAL
    COMMON /COM24/ DTOP(100), DBOT(100),DUP(100), DDOWN(100),&
    &VTOP(100), VBOT(100),VUP(100), VDOWN(100)
    COMMON /COM25/ CPL(100) , CPU(100) , IDLA
    COMMON /COM26/ PJUMP(100)
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM32/ BIGRL    , IRL     , JRL
    COMMON /COM33/ THETA(100,100)
    COMMON /COM34/ NWDGE    , WSLP(100,2)       , XSHK(2,3)         ,&
    &THAMAX(2,3)        , AM1(2,3), ZETA(2,3)         ,&
    &NVWPRT(2), WCONST  , REYNLD  , NISHK

1001 FORMAT(4X,69(1H*))
1002 FORMAT(4X,1H*,67X,1H*)
1003 FORMAT(4X,1H*,25X,15H PROGRAM TSFOIL,27X,1H*/&
    &4X,1H*,29X, 7H SOLVES,31X,1H*/&
    &4X,1H*, 5X,'INVISCID FLOW PAST THIN TWO DIMENSIONAL LIFTING',&
    &' AIRFOIL',7X,1H*/&
    &4X,1H*,29X, 6H USING,32X,1H*/&
    &4X,1H*,15X,35H TRANSONIC SMALL DISTURBANCE THEORY,17X,1H*/&
    &4X,1H*, 9X,'FULLY CONSERVATIVE FINITE DIFFERENCE EQUATIONS',&
    &12X,1H*/&
    &4X,1H*,17X,31H SUCCESSIVE LINE OVERRELAXATION,19X,1H*)
1004 FORMAT(4X,1H*,27X,11H WRITTEN BY,29X,1H*)
1005 FORMAT(4X,1H*,15X,36H EARLL M. MURMAN AND FRANK R. BAILEY,16X,1H*&
    &/     4X,1H*,19X,26H NASA-AMES RESEARCH CENTER,22X,1H*/&
    &4X,1H*,19X,26H MOFFETT FIELD, CALIFORNIA,22X,1H*/&
    &4X,1H*,31X, 4H AND,32X,1H*/&
    &4X,1H*,23X,20H MARGARET L. JOHNSON,24X,1H*/&
    &4X,1H*,18X,30H COMPUTER SCIENCES CORPORATION,19X,1H*/&
    &4X,1H*,20X,26H MOUNTAIN VIEW, CALIFORNIA,21X,1H*/&
    &4X,1H*,67X,1H*/&
    &4X,1H*,12X,43H Documented in NASA SP-347 and NASA CR-3064&
    &,12X,1H*)
1006 FORMAT(4X,1H*,27X,12H Version for, 28X,1H*/&
    &4X,1H*,15X,36H VT Aerospace Design Software Series,16X,1H*/&
    &4X,1H*,67X,1H*/&
    &4X,1H*,15X,14H Contact info:,38X,1H*/&
    &4X,1H*,15X,21H Dr. William H. Mason,31X,1H*/&
    &4X,1H*,15X,45H Aerospace & Ocean Engineering, Virginia Tech&
    &,7X,1H*/&
    &4X,1H*,15X, 20H Blacksburg, VA24061, 32X,1H*/&
    &4X,1H*,15X, 22H Email: whmason@vt.edu,30X,1H*)

    OPEN (UNIT=15, FILE='tsfoil2.out')
    OPEN (UNIT=16, FILE='smry.out')
    OPEN (UNIT=17, FILE='cpxs.out')
    OPEN (UNIT=18, FILE='mmap.out')
    OPEN (UNIT=19, FILE='cnvg.out')
    OPEN (UNIT=20, FILE='mesh.out')
    OPEN (UNIT=21, FILE='cpmp.out')

    WRITE (15,1001)
    WRITE (15,1002)
    WRITE (15,1003)
    WRITE (15,1002)
    WRITE (15,1004)
    WRITE (15,1002)
    WRITE (15,1005)
    WRITE (15,1002)
    WRITE (15,1006)
    WRITE (15,1002)
    WRITE (15,1001)

!
!                  THE MAIN PROGRAM DOES NO COMPUTATIONS
!                  ALL COMPUTATIONS ARE DONE IN SUBROUTINES CALLED BY
!                  TSFOIL.
!                  SUBROUTINE ECHINP PROVIDES A LISTING OF ALL DATA
!                  CARDS FOR ENTIRE JOB.  CAN BE DELETED, IF DESIRED.
!     CALL ECHINP
!                  SUBROUTINE READIN READS ALL INPUT AND CHECKS IT
1   CONTINUE
    CALL READIN
!
!                  SUBROUTNIE SCALE RESCALES ALL PHYSICAL VARIABLES TO
!                  TRANSONIC SIMILARITY FORM
    CALL SCALE
!
!                  SUBROUTINE FARFLD SETS FAR FIELD BOUNDARY CONDITIONS.
    CALL FARFLD
!
!                  SUBROUTINE BODY COMPUTES AIRFOIL GEOMETRY AND PRINTS
!                  OUT GEOMETRICAL INFORMATION
    CALL BODY
!
!                  SUBROUTINE CUTOUT REMOVES MESH POINTS FROM THE INPUT
!                  MESH. CALCULATIONS ARE DONE FIRST ON COARSE MESH,
!                  AND THEN ON PROGRESSIVELY REFINED MESHES UNTIL
!                  INPUT MESH IS ACHIEVED.
    CALL CUTOUT
!
!                  SUBROUTINE GUESSP INITIALIZES P ARRAY
    CALL GUESSP
!
!                  SUBROUTINE DIFCOE CALCULATES FINITE DIFFERENCE
!                  EQUATION COEFFICIENTS WHICH DEPEND ON MESH SIZE.
    CALL DIFCOE
!
!                  SUBROUTINE SETBC  ADJUSTS THE AIRFOIL SLOPE BOUNDARY
!                  CONDITION FOR THE CURRENT X Y MESH. ALSO, THE LIMITS
!                  ON I AND J INDICIES FOR SOLVING THE DIFFERENCE
!                  EQUATIONS ARE SET.
    CALL SETBC(0)
!
!                  SUBROUTINE SOLVE EXECUTES THE MAIN RELAXATION
!                  SOLUTION OF THE DIFFERENCE EQUATIONS
    CALL SOLVE
!
!                  IF FINAL MESH HAS BEEN REACHED, RESULTS ARE PRINTED
!                  OUT IN FINAL FORM.  IF NOT, INTERMEDIATE RESULTS
!                  ARE PRINTED OUT AND THE MESH IS REFINED.  THE ABOVE
!                  SEQUENCE OF CALCULATIONS ARE THEN REPEATED
    IF(IREF .LE. 0) GO TO 5
    IF (ABORT1) GO TO 5
!
!                  SUBROUTINE PRINT1 PRINTS OUT BODY PRESSURE
!                  DISTRIBUTION.
    CALL PRINT1
    IF (ABORT1) GO TO 1
!
!                  SUBROUTINE REFINE  ADDS MESH POINTS
    CALL REFINE
!
!                  REPEAT SEQUENCE OF RELAXATION CALCULATIONS
    CALL DIFCOE
    CALL SETBC(0)
    CALL SOLVE
!
!                  IF FINAL MESH HAS BEEN REACHED, RESULTS ARE PRINTED
!                  OUT IN FINAL FORM.  IF NOT, INTERMEDIATE RESULTS
!                  ARE PRINTED OUT AND THE MESH IS REFINED.  THE ABOVE
!                  SEQUENCE OF CALCULATIONS ARE THEN REPEATED
    IF(IREF .LE. 0) GO TO 5
    IF (ABORT1) GO TO 5
    CALL PRINT1
    CALL REFINE
    CALL DIFCOE
    CALL SETBC(0)
    CALL SOLVE
!
!                  RELAXATION SOLUTION IS COMPLETED
5   CONTINUE
!
!                  PRINTOUT FINAL INFORMATION
    CALL PRINT
    IF (IREF .GT. 0 ) CALL REFINE
    IF (IREF .GT. 0 ) CALL REFINE
    IF (IDLA.NE.0) CALL DLAOUT(ILE,ITE,ALPHA,DELFLP,EMACH,&
    &VFACT,REYNLD)
!
!                  STORE SOLUTION FOR NEXT CASE OR ON TAPE 3
    CALL SAVEP
!
!                  RETURN TO READIN TO COMPUTE NEXT CASE OR TERMINATE
!                  CALCULATIONS.

    CLOSE (UNIT=15)
    CLOSE (UNIT=16)
    CLOSE (UNIT=17)
    CLOSE (UNIT=18)
    CLOSE (UNIT=19)
    CLOSE (UNIT=20)
    CLOSE (UNIT=21)

END PROGRAM

SUBROUTINE ANGLE
!                   SUBROUTINE TO COMPUTE THE ANGLE  THETA AT
!                   EACH MESH POINT.
!                   CALLED BY - FARFLD.
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    REAL JET
    COMMON /COM16/ ALPHA0   , ALPHA1  , ALPHA2  , XSING   ,&
    &OMEGA0   , OMEGA1  , OMEGA2  , JET
    COMMON /COM33/ THETA(100,100)

    R2PI = 1.0 / TWOPI
    DO 20 I=IMIN,IMAX
        XX = XIN(I) - XSING
        DO 10 J=JMIN,JMAX
            YY = YIN(J) * RTK
            R = SQRT(YIN(J)**2 + XX*XX)
            ATN = ATAN2(YY,XX)
            Q = PI - SIGN(PI,YY)
            THETA(J,I) = -(ATN + Q) * R2PI
            IF ( R .LE. 1.0 ) THETA(J,I) = THETA(J,I) * R
10      CONTINUE
20  CONTINUE
    RETURN
END

FUNCTION ARF (X)
!
!              EVALUATES ERF WITH AN ERROR .LT. 1.5E-7 BY RATIONAL
!              APPROXIMATION 7.1.26 FO HANDBOOK OF MATH. FUNCTIONS
!              U. S. DEPT. OF COMMERCE.  NBS APPL MATH SER 55.
!              CALLED BY - AYMESH.
!
    DIMENSION C(5)
    DATA C /1.061405429,-1.453152027,1.421413741, -.284496736,&
    &.254829592/
!
    Y = X
    IF ( X .LT. 0.0 ) Y = -Y
    IF ( Y .LT. 10. ) GO TO 10
    ARF = 1.0
    GO TO 30
10  CONTINUE
    T   = 1.0 / (1.0 + .3275911*Y)
    POLY = 0.0
    DO 20 I=1,5
        POLY = (POLY + C(I)) * T
20  CONTINUE
    ARF = 1.0 - POLY * EXP(-Y*Y)
30  CONTINUE
    IF (X .LT. 0.0) ARF = -ARF
    RETURN
END

SUBROUTINE AYMESH
!                   COMPUTES ANALYTICAL X AND Y MESH POINTS.
!                   CALLED BY - READIN.
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    INTEGER        PSTART
    LOGICAL        PSAVE
    COMMON /COM11/ ALPHAO   , CLOLD   , DELTAO  , DUBO    , EMACHO  ,&
    &IMINO    , IMAXO   , IMAXI   , JMINO   , JMAXO   ,&
    &JMAXI    , PSAVE   , PSTART  ,TITLE(20),TITLEO(20),&
    &VOLO     , XOLD(100),YOLD(100)
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM30/ BXH(401) , REST(3)
!
    DIMENSION  XH(401) , BX(100)
    DATA  A0/.225/, A1/1.4/, A2/1.6/, A3/.6188/, A4/.75/, A5/30./,&
    &A6/.603/, A7/2.0/
    DATA CT1/2.0/, CT2/2.0/, CT3/1.0/, CF1/1.0/, CF2/1.0/, CF3/5.2/
!
    IF (IMAXI .EQ. 77) IMAXI=81
    IMA = (IMAXI - 1) / 2
    A22 = A2 * A2
    A72 = A7 * A7
    FACT = HALFPI * .005
    DO 10 I=201,400
        EX     = TAN(FACT * (I-201) )
        XH(I)  = EX
        EX2    = EX * EX
        EX22   = A22 * EX2
        EX72   = A72 * EX2
        IF (EX72 .LT. 173.0) GO TO 4
        TEX7   = 0.0
        IF (EX22 .LT. 173.0) GO TO 6
        TEX2   = 0.0
        GO TO 8
4       TEX7   = 1.0/EXP(EX72)
6       TEX2   = 1.0/EXP(EX22)
8       BXH(I) = A1*EX*TEX2 + (1.0-TEX7)*ARF(A4*EX)
10  CONTINUE
    XH(401) = 1.E30
    BXH(401) = 1.0
    DO 20 I=1,200
        XH(I)  = - XH(402-I)
        BXH(I) = -BXH(402-I)
20  CONTINUE
    TOOPI = 2.0 / PI
!
!                   ADD 2/PI ATAN(AK(X+A6)) TO BXH TO GIVE MORE
!                   POINTS NEAR THE LEADING EDGE.
!
    DO 30 I=1,401
        BXH(I) = BXH(I) * (1.-A0) + TOOPI*A0*ATAN(A5*(XH(I)+A6))
30  CONTINUE
!
    DX   = 1.0 / IMA
    IM2  = IMA * 2
    IM2P1= IM2 + 1
!
    DO 42 I=1,IM2P1
        IF (I .EQ. 1 .OR. I .EQ. IM2P1) GO TO 31
        BX(I) = (I-1) * DX - 1.0
        GO TO 32
31      CONTINUE
        IF (I .EQ. 1) BX(I) = -.999
        IF (I .EQ. IM2P1) BX(I) = .999
32      CONTINUE
        J     = 0
33      CONTINUE
        J     = J + 1
        IF (BX(I) .GT. BXH(J)) GO TO 33
        IF (BX(I) .LT. BXH(J)) GO TO 34
        XI    = XH(J)
        GO TO 40
!
34      CONTINUE
        BT1  = BX(I)  - BXH(J-1)
        BHT1 = BXH(J) - BXH(J-1)
        XHT1 = XH(J)  - XH(J-1)
        T1   = XHT1 / BHT1
        XI    = XH(J-1) + BT1 * T1
        IF (J .EQ. 2) GO TO 40
!
        BT2  = BX(I)    - BXH(J)
        BHT2 = BXH(J)   - BXH(J-2)
        BHT3 = BXH(J-1) - BXH(J-2)
        XHT2 = XH(J-1)  - XH(J-2)
        T2   = XHT2 / BHT3
        T12  = T1 - T2
        BT12 = BT1 * BT2
        TBT2 = T12 / BHT2
        XI    = XI + BT12 * TBT2
        IF (J .GE. 400) GO TO 40
!
        BT3  = BX(I)    - BXH(J-2)
        BHT4 = BXH(J+1) - BXH(J-2)
        BHT5 = BXH(J+1) - BXH(J)
        BHT6 = BXH(J+1) - BXH(J-1)
        XHT3 = XH(J+1)  - XH(J)
        T3   = XHT3 / BHT5
        XI    = XI + BT12 * BT3/BHT4 * ((T3-T1)/BHT6 - TBT2)
40      CONTINUE
        XIN(I) = XI + A3
42  CONTINUE
    IMAXI = IM2P1
    JM2  = JMAXI
    JMA = JM2 / 2
    FJ = JMA
    IF (BCTYPE .NE. 1) GO TO 44
    C1   = CF1
    C2   = CF2
    C3   = CF3
    GO TO 45
44  CONTINUE
    C1   = CT1
    C2   = CT2
    C3   = CT3
45  CONTINUE
    DETA = 1.0 / (FJ*C1)
    IF (BCTYPE .EQ. 1) DETA = 1.0 / ((FJ+1.0)*C1)
    C    = C3 / (TAN(HALFPI*DETA*FJ))**C2
    DO 50 I=1,JMA
        J    = JMA + I
        ETAI = I * DETA
        YIN(J) = C * (TAN(HALFPI*ETAI))**C2
        YIN(J-2*I+1)  = -YIN(J)
50  CONTINUE
    RETURN
END
BLOCK DATA
!
!                        AND ALL COMMONS WHETHER THEY ARE NEEDED OR NOT.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON / COM5/ XDIFF(100),YDIFF(100)
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
    COMMON / COM8/ CVERGE   , DVERGE  , IPRTER  , MAXIT   ,&
    &WE(3)    , EPS
    INTEGER        BCFOIL
    COMMON / COM9/ BCFOIL   , NL      , NU      , XL(100) , XU(100) ,&
    &YL(100)  , YU(100) , RIGF,IFLAP,DELFLP,FLPLOC
    COMMON /COM10/ YFREE(100),YTUN(100),GAM     , JMXF    , JMXT
    INTEGER        PSTART
    LOGICAL        PSAVE
    COMMON /COM11/ ALPHAO   , CLOLD   , DELTAO  , DUBO    , EMACHO  ,&
    &IMINO    , IMAXO   , IMAXI   , JMINO   , JMAXO   ,&
    &JMAXI    , PSAVE   , PSTART  ,TITLE(20),TITLEO(20),&
    &VOLO     , XOLD(100),YOLD(100)
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        FCR      , KUTTA
    COMMON /COM14/ CLSET    , FCR     , KUTTA   , WCIRC
    COMMON /COM15/ B        , BETA0   , BETA1   , BETA2   , PSI0    ,&
    &PSI1     , PSI2
    REAL JET
    COMMON /COM16/ ALPHA0   , ALPHA1  , ALPHA2  , XSING   ,&
    &OMEGA0   , OMEGA1  , OMEGA2  , JET
    COMMON /COM17/ CYYBLC   , CYYBLD  , CYYBLU  , CYYBUC  , CYYBUD  ,&
    &CYYBUU   ,FXLBC(100),FXUBC(100), ITEMP1, ITEMP2
    LOGICAL        OUTERR
    COMMON /COM18/ ERROR    , I1      , I2      , IERROR  , JERROR  ,&
    &OUTERR   , EMU(100,2)        , VC(100) ,&
    &WI       , DCIRC   , POLD(100,2)
    COMMON /COM19/ DIAG(100), RHS(100), SUB(100), SUP(100)
    COMMON /COM20/ XMID(100), YMID(100)
    COMMON /COM21/ CYYLC    , CYYLD   , CYYLU   , CYYUC   , CYYUD   ,&
    &CYYUU
    COMMON /COM22/ CXC(100) , CXL(100), CXR(100), CXXC(100),CXXL(100),&
    &CXXR(100), C1(100)
    COMMON /COM23/ CYYC(100), CYYD(100),CYYU(100), IVAL
    COMMON /COM24/ DTOP(100), DBOT(100),DUP(100), DDOWN(100),&
    &VTOP(100), VBOT(100),VUP(100), VDOWN(100)
    COMMON /COM25/ CPL(100) , CPU(100) , IDLA
    COMMON /COM26/ PJUMP(100)
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM32/ BIGRL    , IRL     , JRL
    COMMON /COM33/ THETA(100,100)
    COMMON /COM34/ NWDGE    , WSLP(100,2)       , XSHK(2,3)         ,&
    &THAMAX(2,3)        , AM1(2,3), ZETA(2,3)         ,&
    &NVWPRT(2), WCONST  , REYNLD  , NISHK
!
    DATA     WSLP /200*0.0/ , NWDGE / 0 / , WCONST / 4.0 / ,&
    &REYNLD / 4.0E+06 / , ZETA /6*0.0/ , IDLA / 0 /
    DATA     IFLAP / 0 /, DELFLP / 5.0 /, FLPLOC / 0.77 /
    DATA     BCFOIL / 3 / , BCTYPE / 1 /  , PSTART / 1 / ,&
    &PRTFLO / 1 / , SIMDEF / 3 /
    DATA     PHYS /.TRUE. /  , PSAVE / .FALSE. / , FCR  / .TRUE. / ,&
    &KUTTA / .TRUE. /, ABORT1 / .TRUE. /,  AMESH / .FALSE. /
    DATA     EMACH  /.75/ , DELTA /.115/ , ALPHA   /.12/ ,&
    &AK     /0.0/,  GAM/1.4/, RIGF/0.0/, EPS/.2/
    DATA     CLSET /0.0/,  CVERGE/.00001/,  DVERGE/10./,&
    &F     / 0.0 /, H    / 0.0 /  , POR    / 0.0/,&
    &WCIRC/1.0/,  WE/1.8,1.9,1.95/
    DATA     IMAXI/77/,  JMXF/56/,  MAXIT/500/,&
    &NL    / 75  /, NU   / 100 /  , IPRTER / 10 /
    DATA     PI /3.14159265/ , HALFPI /1.570796325/ ,&
    &TWOPI/ 6.28318531/
    DATA    IMIN /1/      , JMIN /1/      , ICUT /2/
    DATA   YIN /100*0.0  /, JMAXI / 64 /  , JMXT   /48 /
    DATA   XIN / -1.075   , -.950   ,&
    &-.825    , -.7     , -.575   , -.45    , -.35    ,&
    &-.25     , -.175   , -.125   , -.075   , -.0525  ,&
    &-.035    , -.0225  , -.015   , -.0075  , -.0025  ,&
    &.0025   ,  .0075  ,  .0125  ,  .0175  ,  .0225  ,&
    &.0275   ,  .0325  ,  .0375  ,  .045   ,  .055   ,&
    &.065    ,  .075   ,  .085   ,  .0975  ,  .115   ,&
    &.140625 ,  .171875,  .203125,  .234375,  .265625,&
    &.296875 ,  .328125,  .359375,  .390625,  .421875,&
    &.453125 ,  .484375,  .515625,  .546875,  .578125,&
    &.609375 ,  .640625,  .671875,  .703125,  .734375,&
    &.765625 ,  .796875,  .828125,  .859375,  .885   ,&
    &.9      , .915    , .93     , .945    , .96     ,&
    &.975    , .99     , 1.0     , 1.01    , 1.025   ,&
    &1.05    , 1.09    , 1.15    , 1.225   , 1.3     ,&
    &1.4     , 1.5     , 1.625   , 1.75    , 1.875   ,&
    &23*0.0 /
    DATA YFREE /  -5.2    , -4.4    , -3.6    , -3.0    , -2.4    ,&
    &-1.95   , -1.6    , -1.35   , -1.15   , -.95    ,&
    &-.80    , -.65    , -.55    , -.45    , -.39    ,&
    &-.34    , -.30    , -.27    , -.24    , -.21    ,&
    &-.18    , -.15    , -.125   , -.1     , -.075   ,&
    &- .05  ,  - .03  ,  - .01  ,    .01  ,    .03  ,&
    &.05  ,    .075 ,    .1   ,    .125 ,    .15  ,&
    &.18  ,    .21  ,    .24  ,    .27  ,    .30  ,&
    &.34  ,    .39  ,    .45  ,    .55  ,    .65  ,&
    &.8   ,    .95  ,   1.15  ,   1.35  ,   1.60  ,&
    &1.95  ,   2.4   ,   3.0   ,   3.6   ,   4.4   ,&
    &5.2   , 44*0.0  /
    DATA   YTUN /  -2.0   ,  -1.8   ,  -1.6   ,  -1.4   ,  -1.2   ,&
    &-1.0   ,   -.8   ,   -.65  ,   -.55  ,  -.45   ,&
    &-.39  ,   -.34  ,   -.30  ,   -.27  ,  -.24   ,&
    &-.21  ,   -.18  ,   -.15  ,   -.125 ,  -.1    ,&
    &- .075 ,  - .05  ,  - .03  ,  - .01  ,    .01  ,&
    &.03  ,    .05  ,    .075 ,    .1   ,    .125 ,&
    &.15  ,    .18  ,    .21  ,    .24  ,    .27  ,&
    &.3   ,    .34  ,    .39  ,    .45  ,    .55  ,&
    &.65  ,    .8   ,   1.0   ,   1.2   ,   1.4   ,&
    &1.6   ,   1.8   ,   2.0   , 52*0.0  /
    DATA  XU  /   0.000008, 0.000167, 0.000391, 0.000799, 0.001407,&
    &0.002153, 0.003331, 0.005336, 0.008648, 0.014583,&
    &0.023481, 0.033891, 0.040887, 0.053973, 0.056921,&
    &0.058456, 0.059966, 0.061445, 0.062909, 0.065925,&
    &0.068785, 0.071482, 0.074007, 0.075322, 0.076603,&
    &0.077862, 0.079112, 0.080445, 0.081819, 0.083269,&
    &0.084841, 0.086702, 0.088848, 0.091378, 0.094413,&
    &0.098308, 0.103104, 0.109010, 0.116244, 0.125452,&
    &0.136635, 0.150037, 0.165853, 0.184699, 0.195177,&
    &0.206361, 0.218244, 0.230813, 0.244047, 0.257917,&
    &0.272371, 0.287410, 0.302990, 0.319057, 0.335555,&
    &0.352421, 0.369591, 0.386995, 0.404133, 0.421391,&
    &0.438708, 0.456013, 0.473246, 0.490343, 0.507242,&
    &0.523881, 0.539536, 0.554867, 0.569823, 0.584351,&
    &0.598405, 0.611936, 0.624904, 0.637273, 0.648435,&
    &0.659016, 0.668987, 0.678321, 0.687012, 0.695090,&
    &0.706936, 0.728406, 0.738649, 0.761390, 0.777010,&
    &0.792241, 0.809068, 0.824992, 0.836953, 0.857188,&
    &0.875621, 0.898268, 0.913686, 0.927686, 0.939804,&
    &0.952002, 0.971789, 0.989100, 0.997860, 1.000000/
    DATA  YU  /   0.000787, 0.003092, 0.004538, 0.006137, 0.007683,&
    &0.009056, 0.010675, 0.012803, 0.015607, 0.019624,&
    &0.024441, 0.029035, 0.031698, 0.035966, 0.036837,&
    &0.037277, 0.037700, 0.038103, 0.038497, 0.039276,&
    &0.039986, 0.040625, 0.041195, 0.041483, 0.041756,&
    &0.042019, 0.042274, 0.042539, 0.042804, 0.043079,&
    &0.043368, 0.043700, 0.044072, 0.044497, 0.044989,&
    &0.045595, 0.046312, 0.047154, 0.048132, 0.049301,&
    &0.050626, 0.052089, 0.053663, 0.055351, 0.056210,&
    &0.057068, 0.057918, 0.058751, 0.059559, 0.060335,&
    &0.061068, 0.061751, 0.062381, 0.062947, 0.063445,&
    &0.063867, 0.064213, 0.064473, 0.064646, 0.064733,&
    &0.064735, 0.064651, 0.064477, 0.064218, 0.063871,&
    &0.063438, 0.062945, 0.062376, 0.061731, 0.061014,&
    &0.060232, 0.059389, 0.058496, 0.057562, 0.056650,&
    &0.055721, 0.054791, 0.053867, 0.052965, 0.052086,&
    &0.050722, 0.048045, 0.046680, 0.043441, 0.041053,&
    &0.038606, 0.035768, 0.032958, 0.030775, 0.026954,&
    &0.023361, 0.018848, 0.015750, 0.012954, 0.010567,&
    &0.008213, 0.004559, 0.001620, 0.000293, 0.000000/
    DATA  XL  /   0.000000, 0.000012, 0.000043, 0.000183, 0.000249,&
    &0.000348, 0.000455, 0.000680, 0.001011, 0.001481,&
    &0.001875, 0.002316, 0.003055, 0.004201, 0.004747,&
    &0.005779, 0.007035, 0.008265, 0.009969, 0.012286,&
    &0.015346, 0.019276, 0.025335, 0.029379, 0.039095,&
    &0.052516, 0.062469, 0.073329, 0.085290, 0.099822,&
    &0.118563, 0.140987, 0.167184, 0.202933, 0.228511,&
    &0.247895, 0.263995, 0.282047, 0.297045, 0.310147,&
    &0.324075, 0.344872, 0.363502, 0.387644, 0.404492,&
    &0.426308, 0.450016, 0.475378, 0.521837, 0.549843,&
    &0.578612, 0.605305, 0.623479, 0.642152, 0.657543,&
    &0.671212, 0.690340, 0.708891, 0.726684, 0.746683,&
    &0.768502, 0.784892, 0.801149, 0.819187, 0.838548,&
    &0.858817, 0.879431, 0.903723, 0.926504, 0.943652,&
    &0.958668, 0.973623, 0.986187, 0.996582, 1.000000,&
    &25*0.0/
    DATA  YL  /   0.000000,-0.000700,-0.001385,-0.002868,-0.003330,&
    &-0.003880,-0.004379,-0.005199,-0.006133,-0.007183,&
    &-0.007933,-0.008676,-0.009776,-0.011204,-0.011815,&
    &-0.012861,-0.013983,-0.014962,-0.016175,-0.017636,&
    &-0.019336,-0.021258,-0.023836,-0.025373,-0.028634,&
    &-0.032423,-0.034840,-0.037182,-0.039456,-0.041862,&
    &-0.044483,-0.047017,-0.049298,-0.051443,-0.052406,&
    &-0.052859,-0.053062,-0.053117,-0.053027,-0.052849,&
    &-0.052562,-0.051951,-0.051218,-0.050013,-0.049004,&
    &-0.047495,-0.045601,-0.043288,-0.038336,-0.034916,&
    &-0.031104,-0.027333,-0.024661,-0.021854,-0.019517,&
    &-0.017429,-0.014527,-0.011771,-0.009228,-0.006537,&
    &-0.003868,-0.002086,-0.000524, 0.000950, 0.002227,&
    &0.003224, 0.003885, 0.004212, 0.004067, 0.003657,&
    &0.003067, 0.002242, 0.001329, 0.000376, 0.000000,&
    &25*0.0/
END

SUBROUTINE BCEND
!                  SUBROUTINE BCEND MODIFIES THE DIAG AND RHS VECTORS
!                  ON EACH I LINE IN THE APPROPRIATE WAY TO INCLUDE THE
!                  BOUNDARY CONDITIONS AT JBOT AND JTOP.
!                  CALLED BY - SYOR.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON / COM5/ XDIFF(100),YDIFF(100)
    COMMON /COM19/ DIAG(100), RHS(100), SUB(100), SUP(100)
    COMMON /COM23/ CYYC(100), CYYD(100),CYYU(100), IVAL
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
    I = IVAL
!                  BRANCH TO APPROPRIATE ADDRESS FOR BCTYPE
    GO TO (10,20,30,40,50,60) , BCTYPE
!                  BCTYPE = 1, FREE AIR
10  CONTINUE
!                  DIRCHLET BOUNDARY CONDITION FOR SUBSONIC FREESTREAM
    IF (AK .GT. 0.0) RETURN
!                  NEUMAN BOUNDARY CONDITION FOR SUPERSONIC FREESTREAM
    DFACL = -CYYD(JBOT) * RTK * XDIFF(I)
    DFACU = -CYYU(JTOP) * RTK * XDIFF(I)
    RFACL = DFACL * (P(JMIN,I) - P(JMIN,I-1))
    RFACU = DFACU * (P(JMAX,I) - P(JMAX,I-1))
    GO TO 95
!                  BCTYPE = 2, SOLID WALL
20  CONTINUE
!                  NEUMAN BOUNDARY CONDITION = 0.
!                  NO MODIFICATION NECESSARY TO DIAG OR RHS
    RETURN
!                  BCTYPE = 3, FREE JET
30  CONTINUE
!                  DIRCHLET BOUNDARY CONDITION
    IF (AK .LT. 0.0) GO TO 31
    PJMIN = -.75 * CIRCFF
    PJMAX = -.25 * CIRCFF
    GO TO 32
31  CONTINUE
    PJMIN = 0.0
    PJMAX = 0.0
32  CONTINUE
    GO TO 90
!                  BCTYPE = 4, IDEAL SLOTTED WALL
40  CONTINUE
!                  NEUMAN BOUNDARY CONDITION
    DFACL =-FHINV * CYYD(JBOT)
    DFACU =-FHINV * CYYU(JTOP)
    IF (AK .LT. 0.0) GO TO 41
    RFACL =  DFACL * ( .75 * CIRCFF + P(JBOT,I))
    RFACU =  DFACU * ( .25 * CIRCFF + P(JTOP,I))
    GO TO 42
41  CONTINUE
    RFACL = DFACL * P(JBOT,I)
    RFACU = DFACU * P(JTOP,I)
42  CONTINUE
    GO TO 95
!                  BCTYPE = 5,  POROUS/PERFORATED WALL
50  CONTINUE
    IF(POR .GT. 1.5) GO TO 55
!                  NEUMAN BOUNDARY CONDITION FOR POR .LT. 1.5
    DFACL = -CYYD(JBOT) * POR * XDIFF(I)
    DFACU = -CYYU(JTOP) * POR * XDIFF(I)
    RFACL = DFACL * (P(JMIN,I) - P(JMIN,I-1))
    RFACU = DFACU * (P(JMAX,I) - P(JMAX,I-1))
    GO TO 95
55  CONTINUE
!                  DIRCHLET BOUNDARY CONDITION FOR POR .GT. 1.5
    IF (I .NE. IUP) RETURN
!                  SET VALUES OF P ON BOUNDARY BY INTEGRATING PX USING
!                  OLD VALUES OF POTENTIAL
    PJMIN = P(JMIN,IUP)
    TERM = -.5 / (POR * (Y(JMIN) - Y(JMIN+1)))
    DO 57 II=IUP,IDOWN
        P(JMIN,II) = P(JMIN,II-1) - TERM * (X(II)-X(II-1)) *&
        &(P(JMIN,II)+P(JMIN,II-1)-P(JMIN+1,II)-P(JMIN+1,II-1))
57  CONTINUE
    PJMAX = P(JMAX,IUP)
    TERM = .5 / (POR * (Y(JMAX) - Y(JMAX-1)))
    DO 58 II=IUP,IDOWN
        P(JMAX,II) = P(JMAX,II-1) - TERM*(X(II) - X(II-1)) *&
        &(P(JMAX,II)+P(JMAX,II-1)-P(JMAX-1,II)-P(JMAX-1,II-1))
58  CONTINUE
    RHS(JBOT) = RHS(JBOT) - (CYYD(JBOT)*(P(JBOT-1,I)-PJMIN))
    RHS(JTOP) = RHS(JTOP) - (CYYU(JTOP)*(P(JTOP+1,I)-PJMAX))
    RETURN
!                  BCTYPE = 6,  GENERAL WALL BOUNDARY CONDITION
!                  DIFFERENCE EQUATIONS FOR THIS BOUNDARY CONDITION
!                  HAVE NOT YET BEEN WORKED OUT.  USER MUST INSERT
!                  INFORMATION NEEDED FOR CALCULATION
60  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1000)
    WRITE(15,1000)

1000 FORMAT(34H1ABNORMAL STOP IN SUBROUTINE BCEND/&
    &24H BCTYPE=6 IS NOT USEABLE)
    STOP
!                  DIRCHLET BOUNDARY CONDITIONS
90  CONTINUE
    RHS(JBOT) = RHS(JBOT) - (CYYD(JBOT)*(PJMIN-P(JBOT-1,I)))
    RHS(JTOP) = RHS(JTOP) - (CYYU(JTOP)*(PJMAX-P(JTOP+1,I)))
    RETURN
95  CONTINUE
!                  NEUMAN BOUNDARY CONDITIONS
    DIAG(JBOT) = DIAG(JBOT) + DFACL
    DIAG(JTOP) = DIAG(JTOP) + DFACU
    RHS(JBOT) = RHS(JBOT) - RFACL + CYYD(JBOT)*P(JBOT-1,I)
    RHS(JTOP) = RHS(JTOP) - RFACU + CYYU(JTOP)*P(JTOP+1,I)
    RETURN
END

SUBROUTINE BODY
!                  COMPUTES BODY GEOMETRY INFORMATION FOR BOUNDARY
!                  CONDITIONS AND OUTPUT INFORMATION.  Five CHOICES OF
!                  BODY DESCRIPTION ARE AVAILABLE AS FOLLOWS
!                       BCFOIL = 1  NACA 00XX AIRFOIL
!                       BCFOIL = 2  PARABOLIC ARC AIRFOIL
!                       BCFOIL = 3  AIRFOIL ORDINATES READ IN
!                       BCFOIL = 4  JAMESON'S AIRFOIL INPUT FORMAT
!                       BCFOIL = 5  diamond airfoil
!                  BODY ORDINATES AND SLOPES ARE COMPUTED AT THE INPUT
!                  X MESH LOCATIONS AND ARE DIVIDED BY THE THICKNESS
!                  RATIO DELTA.  THE BODY VOLUME, CAMBER, AND THICKNESS
!                  ARE ALSO COMPUTED.
!                  ACTUAL BOUNDARY CONDITION IS SET IN SUBROUTINE SETBC
!                  CALLED BY - TSFOIL.
!
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    INTEGER        BCFOIL
    COMMON / COM9/ BCFOIL   , NL      , NU      , XL(100) , XU(100) ,&
    &YL(100)  , YU(100) , RIGF,IFLAP,DELFLP,FLPLOC
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    common /com99/ iread
    COMMON /SPLN/ A(200)    , B(200)  , DY1     , DY2     , K1      ,&
    &K2        , XP      , YP      ,DYP
!
!                  SET NUMBER OF POINTS ON AIRFOIL
    IFOIL = ITE - ILE + 1
!                  ZERO ALL THICKNESSES AND SLOPES
    DO 10 I=IMIN,IMAX
        FU(I) = 0.
        FL(I) = 0.
        FXU(I) = 0.
        FXL(I) = 0.
10  CONTINUE
!                  BRANCH TO APPROPRIATE AIRFOIL SPECIFICATION
    GO TO (100,200,300,400,250), BCFOIL
100 CONTINUE
!                  BCFOIL = 1
!                  FORMULA FOR NACA 00XX SHAPE
    IC        = 0
    DO 125  I = ILE,ITE
        IC        = IC + 1
        Z         = XIN(I)
        XFOIL(IC) = Z
        RTZ       = SQRT(Z)
        Z2        = Z*Z
        Z3        = Z*Z2
        Z4        = Z*Z3
        FU(IC)    = 1.4845*RTZ - .63*Z - 1.758*Z2 + 1.4215*Z3 - .5075*Z4
        FL(IC)    = -FU(IC)
        FXU(IC)   = .74225/RTZ - .63 - 3.516*Z + 4.2645*Z2 - 2.03*Z3
        FXL(IC)   = -FXU(IC)
125 CONTINUE
    GO TO 500
200 CONTINUE
!                  BCFOIL = 2
!                  PARABOLIC ARC AIRFOIL*******BI-CONVEX.
    IC        = 0
    DO 225  I = ILE,ITE
        IC        = IC + 1
        Z         = XIN(I)
        XFOIL(IC) = Z
        Z2        = Z*Z
        FU(IC)    = 2.*(Z - Z2)
        FL(IC)    =  -FU(IC)
        FXU(IC)   = 2. - 4.*Z
        FXL(IC)   = -FXU(IC)
225 CONTINUE
    go to 500

!     mod to treat diamond airfoil

250 continue

!              bcfoil = 5
!              diamond airfoil

    ic        = 0
    do 265 i  = ile,ite
        ic        = ic + 1
        z         = xin(i)
        xfoil(ic) = z
        if (z .le. 0.5) fu(ic) =    .5*z
        if (z .gt. 0.5) fu(ic) = (1. - z)
        fl(ic)    = -fu(ic)
        if (z .le. 0.5) fxu(ic) =  1.0
        if (z .gt. 0.5) fxu(ic) = -1.0
        fxl(ic)    = -fxu(ic)
265 continue

    GO TO 500

300 CONTINUE
!                  BCFOIL = 3
!                  BODY ORDINATES READ IN NAMELIST
    DELINV = 1.
    IF(PHYS) DELINV = 1./DELTA
!                  COMPUTE ORDINATES AND SLOPES AT X MESH LOCATION ON
!                  AIRFOIL BY CUBIC SPLINE INTERPOLATION.
!                  DERIVATIVE END CONDITIONS ARE SPECIFIED AT X=0
!                  AND X=1
    K1 = 1
    K2 = 1
!                                  UPPER SURFACE
!                  CALCULATE DY/DX AT END POINTS BY FINITE DIFFERENCE
!                  FORMULA.
    DY1 = (YU(2) - YU(1)) / (XU(2) - XU(1))
    DY2 = (YU(NU) - YU(NU-1)) / (XU(NU) - XU(NU-1))
!                  INITIALIZE CUBIC SPLINE INTERPOLATION
    CALL SPLN1(XU,YU,NU)
!                  CALCULATE ORDINATES AND SLOPES
    IC = 0
    DO 320 I=ILE,ITE
        IC = IC + 1
        XP = XIN(I)
        XFOIL(IC) = XP
        CALL SPLN1X(XU,YU,NU)
!                  SET ORDINATE AND SLOPE OF AIRFOIL TO INTERPOLATED
!                  VALUE DIVIDED BY THICKNESS RATIO
        FU(IC) = YP*DELINV
        FXU(IC) = DYP*DELINV
320 CONTINUE
!                                 LOWER SURFACE
!                  CALCULATE DY/DX AT END POINTS BY FINITE DIFFERENCE
!                  FORMULA.
    DY1 = (YL(2) - YL(1)) / (XL(2) - XL(1))
    DY2 = (YL(NL) - YL(NL-1)) / (XL(NL) - XL(NL-1))
!                  INITIALIZE CUBIC SPLINE INTERPOLATION
    CALL SPLN1(XL,YL,NL)
!                  CALCULATE ORDINATES AND SLOPES
    IC = 0
    DO 330 I=ILE,ITE
        IC = IC + 1
        XP = XIN(I)
        CALL SPLN1X(XL,YL,NL)
!                  SET ORDINATE AND SLOPE OF AIRFOIL TO INTERPOLATED
!                  VALUE DIVIDED BY THICKNESS RATIO
        FL(IC) = YP* DELINV
        FXL(IC) = DYP*DELINV
330 CONTINUE
    GO TO 500
400 CONTINUE
!                  BCFOIL = 4
!                  AIRFOIL INPUT IN JAMESON'S FORMAT
    K1 = 1
    K2 = 1
    DELINV = 1.
    IF (PHYS) DELINV = 1./DELTA
    READ(iread,470)
    READ(iread,480) FSYM,FNU,FNL
    ISYM = FSYM
    NU = FNU
    NL = FNL
    READ(iread,470)
    READ(iread,490) (XU(N),YU(N),N=1,NU)
    DY1 = (YU(2) - YU(1)) / (XU(2) - XU(1))
!     Bug: N = NU + 1, and therefore, the derivative is wrong - Andy Ko 4/9/03
!      DY2 = (YU(N) - YU(N-1)) / (XU(N) - XU(N-1))
    DY2 = (YU(NU) - YU(NU-1)) / (XU(NU) - XU(NU-1))
    CALL SPLN1(XU,YU,NU)
    IC = 0
    DO 410 I=ILE,ITE
        IC = IC + 1
        XP = XIN(I)
        XFOIL(IC) = XP
        CALL SPLN1X(XU,YU,NU)
        FU(IC) = YP * DELINV
        FXU(IC) = DYP * DELINV
410 CONTINUE
    IF (ISYM.EQ.0) GO TO 440
    NL = NU
    IC = 0
    DO 420 I=ILE,ITE
        IC = IC + 1
        FL(IC) = -FU(IC)
420 FXL(IC) = -FXU(IC)
    DO 430 N=1,NL
        XL(N) = XU(N)
430 YL(N) = -YU(N)
    GO TO 500
440 CONTINUE
    READ(iread,470)
    READ(iread,490) (XL(N),YL(N),N=1,NL)
    DY1 = (YL(2) - YL(1)) / (XL(2) - XL(1))
!     Bug: N = NL + 1 and therefore derivative is calculated wrong - Andy Ko 4/9/03
!      DY2 = (YL(N) - YL(N-1)) / (XL(N) - XL(N-1))
    DY2 = (YL(NL) - YL(NL-1)) / (XL(NL) - XL(NL-1))
    CALL SPLN1(XL,YL,NL)
    IC = 0
    DO 450 I=ILE,ITE
        IC = IC + 1
        XP = XIN(I)
        CALL SPLN1X(XL,YL,NL)
        FL(IC) = YP * DELINV
        FXL(IC) = DYP * DELINV
450 CONTINUE
470 FORMAT(1X)
480 FORMAT(3F10.0)
490 FORMAT(2F10.0)
500 CONTINUE
!                  EXECUTE  CALCULATIONS COMMON TO ALL AIRFOILS
!
!                  COMPUTE AIRFOIL VOLUMNE
    CALL SIMP(VOLU,XFOIL,FU,IFOIL,IERR)
    CALL SIMP(VOLL,XFOIL,FL,IFOIL,IERR)
    VOL = VOLU - VOLL
!                  ADD IN FLAP DEFLECTION
    IF (IFLAP .EQ. 0) GO TO 524
    DFLAP = DELFLP/57.29578
    SDFLAP = SIN(DFLAP)
    DO 505 I=1,IFOIL
        IF (XFOIL(I) .GE. FLPLOC) GO TO 510
505 CONTINUE
    GO TO 524
510 IFP      = I
    DO 520 I = IFP,IFOIL
        DELY     = (XFOIL(I)-FLPLOC)*SDFLAP*DELINV
        FU(I)    = FU(I)  - DELY
        FL(I)    = FL(I)  - DELY
        FXU(I)   = FXU(I) - DFLAP*DELINV
        FXL(I)   = FXL(I) - DFLAP*DELINV
520 CONTINUE
!                  COMPUTE CAMBER AND THICKNESS
524 DO 525  I = 1,IFOIL
        CAMBER(I) = .5*(FU(I) + FL(I))
        THICK(I)  = .5*(FU(I) - FL(I))
525 CONTINUE
    DO 530 I = 1,IFOIL
        FXU(I)   = FXU(I) / SQRT(1.0 + RIGF *(DELTA*FXU(I))**2 )
        FXL(I)   = FXL(I) / SQRT(1.0 + RIGF *(DELTA*FXL(I))**2 )
530 CONTINUE
    CALL PRBODY
    RETURN
END

SUBROUTINE CDCOLE
!                  COMPUTES THE DRAG BY MOMENTUM INTEGRAL METHOD.
!                  INTEGRATES AROUND A CONTOUR ENCLOSING THE BODY AND
!                  ALONG ALL SHOCKS INSIDE THE CONTOUR
!                  CALLED BY - PRINT.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    COMMON/COM30/ Z(100), ARG(100), REST(204)
    GAM123 = GAM1*2./3.
!                  SET LOCATIONS OF CONTOUR BOUNDARIES
!
!                  UPSTREAM BOUNDARY
!                             IF AK = 0.0 CDCOLE WILL NOT BE CALLED.
!                                 AMACH MAY NOT BE = 1.0  .
    IF(AK .GT. 0.) IU = (ILE + IMIN)*.5
    IF(AK .LT. 0.) IU = IUP
!                  TOP AND BOTTOM BOUNDARIES
!                  SUBSONIC FREESTREAM
!                  SET JB,JT TO INCLUDE AS MUCH OF SHOCKS AS POSSIBLE
    JT = JMAX - 1
    JB = JMIN + 1
    IF (AK .GT. 0.) GO TO 30
!                  SUPERSONIC FREESTREAM
!                  SET JB,JT TO INCLUDE ONLY SUBSONIC PART OF
!                  DETACHED BOW WAVE
!
!                  FIND BOW SHOCK WAVE
    ISTOP = ILE - 3
    CALL FINDSK(IUP,ISTOP,JUP,IBOW)
    IF(IBOW .LT. 0) GO TO 325
!                  SEARCH UP SHOCK TO FIND TIP OF SUBSONIC REGION
    ISK = IBOW
    JSTART = JUP + 1
    JT = JUP - 1
    DO 10 J = JSTART,JMAX
        JT = JT + 1
        ISKOLD = ISK
        CALL NEWISK(ISKOLD,J, ISK)
        IF(ISK  .LT. 0) GO TO 15
10  CONTINUE
15  CONTINUE
!                  SEARCH DOWN SHOCK TO FIND TIP OF SUBSONIC REGION
    ISK = IBOW
    JB = JLOW + 2
    DO 20 J = JMIN,JLOW
        JJ = JLOW - J + JMIN
        JB = JB - 1
        ISKOLD = ISK
        CALL NEWISK(ISKOLD,JJ,ISK)
        IF(ISK .LT. 0) GO TO 25
20  CONTINUE
25  CONTINUE
!                  SAVE I LOCATION OF BOW SHOCK WAVE ON LOWER BOUNDARY
    IBOW = ISKOLD
30  CONTINUE
!                  DOWNSTREAM BOUNDARY
    ID = (ITE + IMAX) * .5
    IF(PX(ITE+1,JUP) .LT. SONVEL)  GO TO 40
!                  TRAILING EDGE IS SUPERSONIC.  PLACE DOWNSTREAM
!                  BOUNDARY AHEAD OF TRAILING EDGE TO AVOID TAIL SHOCK
    I = ITE
35  CONTINUE
    I = I-1
    IF(X(I) .GT. .75) GO TO 35
    ID = I
40  CONTINUE
!
!                  ALL BOUNDARIES ARE FIXED
!
!                  COMPUTE INTEGRALS ALONG BOUNDARIES
!
!                  INTEGRAL ON UPSTREAM BOUNDARY
    CDUP = 0.
    IF(AK .LT. 0.) GO TO 120
    L = 0
    DO 110 J = JB,JT
        L = L+1
        Z(L) = Y(J)
        U = PX(IU,J)
        V = PY(IU,J)
        ARG(L) =((AK - GAM123*U)*U*U - V*V)*.5
110 CONTINUE
    CALL TRAP(Z,ARG,L,SUM)
    CDUP = 2.*CDFACT*SUM
120 CONTINUE
!                  INTEGRAL ON TOP BOUNDARY
    L = 0
    DO 130 I = IU,ID
        L = L + 1
        Z(L) = X(I)
        ARG(L) = -PX(I,JT)*PY(I,JT)
130 CONTINUE
    CALL TRAP(Z,ARG,L,SUM)
    CDTOP = 2.*CDFACT*SUM
!                  INTEGRAL ON BOTTOM BOUNDARY
    L = 0
    DO 140 I = IU,ID
        L = L +  1
        ARG(L) = PX(I,JB)*PY(I,JB)
140 CONTINUE
    CALL TRAP(Z,ARG,L,SUM)
    CDBOT = 2.*CDFACT*SUM
!                  INTEGRAL ON DOWNSTREAM BOUNDARY
    L = 0
    DO 150 J = JB,JT
        L = L + 1
        Z(L) = Y(J)
        U = PX(ID,J)
!                  IF FLOW SUPERSONIC, USE BACKWARD DIFFERENCE FORMULA
        IF(U .GT. SONVEL) U = PX(ID-1,J)
        V = PY(ID,J)
        ARG(L) = ((GAM123*U - AK)*U*U + V*V)*.5
150 CONTINUE
    CALL TRAP(Z,ARG,L,SUM)
    CDDOWN = 2.*CDFACT*SUM
!                  INTEGRAL ON BODY BOUNDARY
    CDBODY = 0.
    IF(ID .GT. ITE) GO TO 200
    ILIM = ITE + 1
    L = 0
    DO 160 I=ID,ILIM
        IB = I-ILE + 1
        L = L + 1
        Z(L) = X(I)
        UU = CJUP*PX(I,JUP) - CJUP1*PX(I,JUP+1)
        UL = CJLOW*PX(I,JLOW) - CJLOW1*PX(I,JLOW-1)
        ARG(L) = -UU*FXU(IB) + UL*FXL(IB)
160 CONTINUE
    CALL TRAP(Z,ARG,L,SUM)
    CDBODY = 2.*CDFACT*SUM
200 CONTINUE
!
!                  INTEGRATION ALONG SHOCK WAVES
    CDWAVE = 0.
    LPRT1 = 0
    LPRT2 = 0
    NSHOCK = 0
    IF(AK .GT. 0.) GO TO 220
!                  INTEGRATE ALONG DETACHED BOW WAVE
    NSHOCK = NSHOCK + 1
    LPRT1 = 1
    LPRT2 = 1
    L = 0
    ISK = IBOW
    DO 210 J = JB,JT
        L = L + 1
        ISKOLD = ISK
        CALL NEWISK(ISKOLD,J,ISK)
        Z(L) = Y(J)
        ARG(L) = (PX(ISK+1,J) - PX(ISK-2,J))**3
210 CONTINUE
    CALL TRAP(Z,ARG,L,SUM)
    CDSK = -GAM1/6.*CDFACT*SUM
    CDWAVE = CDWAVE + CDSK
    CALL PRTSK(Z,ARG,L,NSHOCK, CDSK,LPRT1)
220 CONTINUE
!                  INTEGRATE ALONG SHOCKS ABOVE AIRFOIL
    ISTART = ILE
225 CONTINUE
    CALL FINDSK(ISTART,ITE,JUP,ISK)
    IF(ISK .LT. 0) GO TO 250
!                  SHOCK WAVE FOUND
    ISTART = ISK + 1
    NSHOCK = NSHOCK + 1
    LPRT1 = 0
    L = 1
    Z(L) = 0.
    ARG(L) = (CJUP*(PX(ISK+1,JUP) - PX(ISK-2,JUP))&
    &- CJUP1*(PX(ISK+1,JUP+1) - PX(ISK-2,JUP+1)))**3
    DO 230 J = JUP , JT
        L = L+1
        Z(L) = Y(J)
        ARG(L) = (PX(ISK+1,J) - PX(ISK-2,J))**3
        ISKOLD = ISK
        JSK = J + 1
        CALL NEWISK(ISKOLD,JSK,ISK)
        IF(ISK .LT. 0) GO TO 240
        IF(ISK .GT. ID) GO TO 235
230 CONTINUE
235 CONTINUE
    LPRT1 = 1
240 CONTINUE
    CALL TRAP(Z,ARG,L,SUM)
    CDSK = -GAM1/6.*CDFACT*SUM
    CDWAVE = CDWAVE + CDSK
    CALL PRTSK(Z,ARG,L,NSHOCK,CDSK,LPRT1)
    IF(LPRT1 .EQ. 1) LPRT2 = 1
!                  RETURN TO FIND NEXT SHOCK
    GO TO 225
!                  INTEGRATE ALONG SHOCKS BELOW AIRFOIL
250 CONTINUE
    ISTART = ILE
260 CONTINUE
    CALL FINDSK(ISTART,ITE,JLOW,ISK)
    IF(ISK .LT. 0) GO TO 300
!                  SHOCK WAVE FOUND
    ISTART = ISK + 1
    NSHOCK = NSHOCK + 1
    LPRT1 = 0
    L = 1
    Z(L) = 0.
    ARG(L) = (CJLOW*(PX(ISK+1,JLOW) - PX(ISK-2,JLOW))&
    &- CJLOW1*(PX(ISK+1,JLOW-1) - PX(ISK-2,JLOW-1)))**3
    DO 270 JJ = JB,JLOW
        J = JLOW + JB - JJ
        L = L+1
        Z(L) = Y(J)
        ARG(L) = (PX(ISK+1,J) - PX(ISK-2,J))**3
        ISKOLD = ISK
        JSK = J - 1
        CALL NEWISK(ISKOLD,JSK,ISK)
        IF(ISK .LT. 0) GO TO 280
        IF(ISK .GT. ID) GO TO 275
270 CONTINUE
275 CONTINUE
    LPRT1 = 1
280 CONTINUE
    CALL TRAP(Z,ARG,L,SUM)
    CDSK = -GAM1/6.*CDFACT*(-SUM)
    CDWAVE = CDWAVE + CDSK
    CALL PRTSK(Z,ARG,L,NSHOCK,CDSK,LPRT1)
    IF(LPRT1 .EQ. 1) LPRT2 = 1
!                  RETURN TO FIND NEXT SHOCK
    GO TO 260
300 CONTINUE
!
!                  INTEGRATION ALONG SHOCKS IS COMPLETE
!
!                  PRINTOUT CD INFORMATION
    XU = X(IU)
    XD = X(ID)
    YT = Y(JT)*YFACT
    YB = Y(JB)*YFACT
    CDC = CDUP + CDTOP + CDBOT + CDDOWN + CDBODY
    CD = CDC + CDWAVE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1001)
    WRITE(15,1001)

!      WRITE(6,1002) XU,CDUP,  XD,CDDOWN,  YT,CDTOP,  YB,CDBOT
!      IF(XD .LT. 1.)  WRITE(6,1003)  XD,CDBODY
!      WRITE(6,1004) CDC
!      WRITE(6,1005) NSHOCK,CDWAVE
!      IF(NSHOCK .GT. 0 .AND. LPRT2 .EQ. 0)  WRITE(6,1007)
!      IF(NSHOCK .GT. 0 .AND. LPRT2 .EQ. 1)  WRITE(6,1008)
!      WRITE(6,1006) CD

    WRITE(15,1002) XU,CDUP,  XD,CDDOWN,  YT,CDTOP,  YB,CDBOT
    IF(XD .LT. 1.)  WRITE(15,1003)  XD,CDBODY
    WRITE(15,1004) CDC
    WRITE(15,1005) NSHOCK,CDWAVE
    WRITE(16,'("TOTAL CDWAVE =" F16.12)') CDWAVE
    IF(NSHOCK .GT. 0 .AND. LPRT2 .EQ. 0)  WRITE(15,1007)
    IF(NSHOCK .GT. 0 .AND. LPRT2 .EQ. 1)  WRITE(15,1008)
    WRITE(15,1006) CD
    WRITE(16,'("TOTAL CD     =" F16.12)') CD

    RETURN
325 CONTINUE
!                  SHOCK IS TOO CLOSE TO BODY TO DO CONTOUR INTEGRAL.
!                  WRITE MESSAGE AND RETURN
    ULE = PX(ILE,JUP)

!     Changed to write to a file - Andy Ko 4/3/03
!      IF(ULE .GT. SONVEL)  WRITE(6,1011)
!      IF(ULE .LE. SONVEL)  WRITE(6,1012)
    IF(ULE .GT. SONVEL)  WRITE(15,1011)
    IF(ULE .LE. SONVEL)  WRITE(15,1012)

    CD = DRAG(CDFACT)
!      WRITE(6,1013) CD
    WRITE(15,1013) CD
    RETURN
1001 FORMAT(60H1CALCULATION OF DRAG COEFFICIENT BY MOMENTUM INTEGRAL ME&
    &THOD)
1002 FORMAT(27H0BOUNDARIES OF CONTOUR USED,15X,18HCONTRIBUTION TO CD/&
    &16H UPSTREAM    X =,F12.6,15X,8HCDUP   =,F12.6/&
    &16H DOWNSTREAM  X =,F12.6,15X,8HCDDOWN =,F12.6/&
    &16H TOP         Y =,F12.6,15X,8HCDTOP  =,F12.6/&
    &16H BOTTOM      Y =,F12.6,15X,8HCDBOT  =,F12.6)
1003 FORMAT(16H BODY AFT OF X =,F12.6,15X,8HCDBODY =,F12.6)
1004 FORMAT(15X,36HTOTAL CONTRIBUTIONS AROUND CONTOUR =,F12.6)
1005 FORMAT(10H0THERE ARE,I3,38H SHOCKS INSIDE CONTOUR. TOTAL CDWAVE =,&
    &F12.6)
1006 FORMAT(51H0DRAG CALCULATED FROM MOMENTUM INTEGRAL    CD     =,&
    &F12.6)
1007 FORMAT(43H0NOTE - ALL SHOCKS CONTAINED WITHIN CONTOUR/&
    &30H CDWAVE EQUALS TOTAL WAVE DRAG)
1008 FORMAT(52H0NOTE - ONE OR MORE SHOCKS EXTEND OUTSIDE OF CONTOUR/&
    &38H CDWAVE DOES NOT EQUAL TOTAL WAVE DRAG)
1011 FORMAT(31H1SHOCK WAVE IS ATTACHED TO BODY/&
    &33H MOMENTUM INTEGRAL CANNOT BE DONE/&
    &45H DRAG OBTAINED FROM SURFACE PRESSURE INTEGRAL/)
1012 FORMAT(41H1DETACHED SHOCK WAVE IS TOO CLOSE TO BODY/&
    &33H MOMENTUM INTEGRAL CANNOT BE DONE/&
    &45H DRAG OBTAINED FROM SURFACE PRESSURE INTEGRAL/)
1013 FORMAT(4H0CD=,F12.6)
END

SUBROUTINE CKMESH
!
!                        CHECK X MESH AND ADJUST TO CONTAIN ODD NO. OF
!                        POINTS BEFORE TAIL AND ODD NO. AFTER TAIL.
!                        ITE IS INCLUDED IN BOTH COUNTS.
!                        CKMESH IS CALLED ONCE BY READIN.
!
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
!
    IF (ICUT .GT. 0) GO TO 4
    IREF = -1
    RETURN
4   CONTINUE
!
!                        TEST TO BE SURE THAT ADJUSTING THE NO. OF
!                        POINTS WONT MAKE IMAX OR JMAX LARGER THAN 100.
    IF (IMAX .LE. 98 .AND. JMAX .LE. 98) GO TO 8
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,100)
    WRITE (15,100)
    IREF = -1
    RETURN
8   CONTINUE
    IF (MOD(ITE-IMIN+1, 2) .NE. 0) GO TO 20
!                        ADD EXTRA MESH POINT AHEAD OF AIRFOIL.
    LP = IMAX + IMIN + 1
    DO 10 I=IMIN,IMAX
        L = LP - I
        XIN(L) = XIN(L-1)
10  CONTINUE
    IMAX = IMAX + 1
    XIN(IMIN) = 2. * XIN(IMIN+1) - XIN(IMIN+2)
    CALL ISLIT ( XIN )
20  CONTINUE
!                        ADD EXTRA MESH POINT AFTER AIRFOIL.
    IF (MOD(IMAX-ITE+1, 2) .NE. 0) GO TO 30
    IMAX = IMAX + 1
    XIN(IMAX) = 2. * XIN(IMAX-1) - XIN(IMAX-2)
30  CONTINUE
!                        CHECK Y MESH AND ADJUST TO CONTAIN EVEN NO. OF
!                        POINTS ABOVE AND BELOW SLIT.
    IF (MOD(JLOW-JMIN ,2) .NE. 0) GO TO 50
!                        ADD EXTRA MESH POINT BELOW SLIT.
    LP = JMAX + JMIN + 1
    DO 40 J=JMIN,JMAX
        L = LP - J
        YIN(L) = YIN(L-1)
40  CONTINUE
    JMAX = JMAX + 1
    YIN(JMIN) = 2. * YIN(JMIN+1) - YIN(JMIN+2)
    CALL JSLIT ( YIN )
50  CONTINUE
!                        ADD EXTRA MESH POINT ABOVE SLIT.
    IF (MOD(JMAX-JUP  ,2) .NE. 0) GO TO 60
    JMAX = JMAX + 1
    YIN(JMAX) = 2.0 * YIN(JMAX-1) - YIN(JMAX-2)
60  CONTINUE
    RETURN
!
100 FORMAT(96H0 THE MESH CANNOT BE ADJUSTED FOR CUTOUT, BECAUSE IMAX O&
    &R JMAX IS TOO CLOSE TO THE LIMIT OF 100./&
    &5X,19HIREF WAS SET TO  0 )
!
END

SUBROUTINE CPPLOT (X, Y, Z, W, NP)
!
!                        SUBROUTINE CPPLOT PRODUCES A PRINTER PLOT
!                        OF CRITICAL PRESSURE VS X .
!                        CALLED BY - FIXPLT.
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
!
    DIMENSION  X(101) , Y(101) , Z(101) , W(101) , M(120), ISYM(8)
    DIMENSION  A(3) , IC(3)
    DATA  IC/ 1, 1024, 1048576/
    DATA ISYM/1H ,1HU,1HL,1HB,1H-,1HU,1HL,1HB/
!                        NC IS THE NUMBER OF COLUMNS.
!                        NR IS THE NUMBER OF ROWS.
    DATA  NC /120/ , NR /50/
!     INITIALIZE RANGES
    IF ( AMESH ) GO TO 3
    NPL = 1
    NPR = NP - 1
    NL5 = 2
    GO TO 4
3   CONTINUE
    NPL = 2
    NPR = NP - 2
    NL5 = 3
4   CONTINUE
    HL = X(NPL)
    HR = X(NPL)
    VB=AMIN1(Y(1),Z(1),W(1))
    VT=AMAX1(Y(1),Z(1),W(1))
!     DETERMINE RANGES
    DO 5 I = NL5,NPR
        HL=AMIN1(HL,X(I))
        HR=AMAX1(HR,X(I))
        VB=AMIN1(VB,Y(I),Z(I),W(I))
        VT=AMAX1(VT,Y(I),Z(I),W(I))
5   CONTINUE
!
!                        SKIP TO NEW PAGE AND WRITE PLOT HEADING.
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,900)
    WRITE(15,900)
    VDEL=(VT-VB)/FLOAT(NR)
    HDEL=(HR-HL)/FLOAT(NC)
    HDELM = 1.0 / HDEL
    VL=VT
!
!
    DO 100 IROW = 1,NR
        VH=VL
        VL=FLOAT(NR-IROW)*VDEL+VB
        DO 15 I=1,NC
15      M(I)=0
!
        DO 30 I =NPL,NPR
            J     = MAX0(1,MIN0(NC,1+INT((X(I)-HL) * HDELM)))
            A(1) = Y(I)
            A(2) = Z(I)
            A(3) = W(I)
            DO 20 K=1,3
                IF (A(K) .GT. VH) GO TO 20
                IF (A(K) .GT. VL .OR. (A(K) .LE. VB .AND. IROW .EQ. NR))&
                &M(J) = M(J) + IC(K)
20          CONTINUE
30      CONTINUE
!
        DO 90 I=1,NC
            J = 1
            IF (M(I) .LT. IC(3)) GO TO 70
            J = J + 4
            M(I) = MOD(M(I),IC(3))
70          CONTINUE
            IF (M(I) .LT. IC(2)) GO TO 75
            J = J + 2
            M(I) = MOD(M(I),IC(2))
75          CONTINUE
            IF (M(I) .LE. 0) GO TO 80
            J = J + 1
80          CONTINUE
            M(I) = ISYM(J)
90      CONTINUE
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,901)   (M(I),I=1,NC)
        WRITE(15,901)   (M(I),I=1,NC)
100 CONTINUE
!
    RETURN
!
900 FORMAT(1H1,34X,50HPRINTER PLOT OF CP ON BODY AND DIVIDING STREAMLI&
    &NE//9X,17H U  FOR CP(UPPER),5X,17H L  FOR CP(LOWER),&
    &5X,27H B  FOR CP(UPPER)=CP(LOWER),5X,18H ---  FOR CP SONIC)
901 FORMAT(1X,120A1)
!
END

SUBROUTINE CUTOUT
!
!                        SUBROUTINE TO REDUCE THE NUMBER OF MESH POINTS
!                        FOR THE FIRST CUT AT SOLUTION. THE X-MESH AND
!                        Y-MESH WILL BE HALVED, AND IF POSSIBLE BE
!                        HALVED AGAIN.
!                             IREF =-1, CUTOUT SHOULDNT HAVE BEEN CALLED
!                             IREF = 0, IF NOT HALVED AT ALL.
!                             IREF = 1, IF HALVED ONCE.
!                             IREF = 2, IF HALVED TWICE.
!                        CALLED BY - TSFOIL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON /COM20/ XMID(100), YMID(100)
!
    IF(IREF .NE. -1) GO TO 5
!                  MESH CANNOT BE REFINED, LOAD XIN,YIN INTO X,Y
    DO 1 I = IMIN,IMAX
        X(I) = XIN(I)
1   CONTINUE
    DO 2 J = JMIN,JMAX
        Y(J) = YIN(J)
2   CONTINUE
    IREF = 0
    RETURN
5   CONTINUE
    K = IMIN - 1
    DO 10 I=IMIN,IMAX,2
        K = K + 1
        XMID(K) = XIN(I)
10  CONTINUE
    IMAX = (IMAX-IMIN) * .5 + IMIN
    CALL ISLIT ( XMID )
    K = JMIN - 1
    JE = JLOW - 1
    DO 15 J=JMIN,JE,2
        K = K + 1
        YMID(K) = YIN(J)
15  CONTINUE
    JST = JUP + 1
    DO 20 J=JST,JMAX,2
        K = K + 1
        YMID(K) = YIN(J)
20  CONTINUE
    JMAX = (JMAX-JMIN) * .5 + JMIN
    CALL JSLIT ( YMID )
    IREF = 1
!                        FIRST HALVING COMPLETE. CHECK IF NO. OF POINTS
!                        IS ODD.
    IF (ICUT .EQ. 1) GO TO 30
    IF (MOD(ITE-IMIN+1, 2) .EQ. 0) GO TO 30
    IF (MOD(IMAX-ITE+1,2) .EQ. 0) GO TO 30
    IF (MOD(JLOW-JMIN ,2) .EQ. 0) GO TO 30
    IF (MOD(JMAX-JUP  ,2) .NE. 0) GO TO 60
30  CONTINUE
!                        ONLY ONE MESH REFINEMENT POSSIBLE.
    DO 40 I=IMIN,IMAX
        X(I) = XMID(I)
40  CONTINUE
    DO 50 J=JMIN,JMAX
        Y(J) = YMID(J)
50  CONTINUE
    RETURN
!                        ALL POINTS ARE ODD SO CUT AGAIN.
60  CONTINUE
    K = IMIN - 1
    DO 70 I=IMIN,IMAX,2
        K = K + 1
        X(K) = XMID(I)
70  CONTINUE
    IMAX = (IMAX-IMIN) * .5 + IMIN
    CALL ISLIT ( X )
    K = JMIN - 1
    JE = JLOW - 1
    DO 75 J=JMIN,JE,2
        K = K + 1
        Y(K) = YMID(J)
75  CONTINUE
    JST = JUP + 1
    DO 80 J=JST,JMAX,2
        K = K + 1
        Y(K) = YMID(J)
80  CONTINUE
    JMAX = (JMAX-JMIN) * .5 + JMIN
    CALL JSLIT ( Y )
    IREF = 2
    RETURN
!
END

SUBROUTINE DIFCOE
!
!  COMPUTE DIFFERENCE COEFFICIENTS IN FIELD
!                  CALLED BY - TSFOIL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON / COM5/ XDIFF(100),YDIFF(100)
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
    COMMON /COM17/ CYYBLC   , CYYBLD  , CYYBLU  , CYYBUC  , CYYBUD  ,&
    &CYYBUU   ,FXLBC(100),FXUBC(100), ITEMP1, ITEMP2
    COMMON /COM22/ CXC(100) , CXL(100), CXR(100), CXXC(100),CXXL(100),&
    &CXXR(100), C1(100)
    COMMON /COM23/ CYYC(100), CYYD(100),CYYU(100), IVAL
!
!                        COEFFICIENTS FOR (P)X AND (P)XX AT IMIN
    CXXL(IMIN) = 0.0
    CXXR(IMIN) = 0.0
    CXXC(IMIN) = 0.0
    CXL (IMIN) = 0.0
    CXR (IMIN) = 0.0
    CXC (IMIN) = 0.0
!  COEFFICIENTS FOR (P)X AND (P)XX FROM I=IMIN+1 TO I=IMAX-1
    C2 = GAM1*.5
    ISTART = IMIN+1
    IEND=IMAX-1
    DO 1 I=ISTART,IEND
        DXL=X(I)-X(I-1)
        DXR=X(I+1)-X(I)
        DXC = .5 * (X(I+1) - X(I-1))
!  FOR VC
        C1(I)=   AK/DXC
!  FOR (P)X
        CXL(I) = -C2 / (DXL*DXC)
        CXR(I) =  C2 / (DXR*DXC)
        CXC(I) = -CXL(I) - CXR(I)
!  FOR (P)XX
        CXXL(I) = 1.0 / DXL
        CXXR(I) = 1.0 / DXR
        CXXC(I) = CXXL(I) + CXXR(I)
1   CONTINUE
!                  COEFFICIENTS FOR (P)X AND (P)XX AT IMAX
    DX           = X(IMAX) - X(IMAX-1)
    Q = 1.0 / (DX*DX)
    C1(IMAX) = AK / DX
    CXL(IMAX)  = -C2 * Q
    CXR(IMAX)  =  C2 * Q
    CXC(IMAX) = 0.0
    CXXL(IMAX) =  1.0 / DX
    CXXR(IMAX) =  1.0 / DX
    CXXC(IMAX) =  CXXL(IMAX) + CXXR(IMAX)
!                                 COEFFICIENTS FOR (P)YY AT JMIN.
!
    DYU = Y(JMIN+1) - Y(JMIN)
    CYYD(JMIN) = 2.0 / DYU
    CYYU(JMIN) = 2./(DYU*DYU)
    CYYC(JMIN) = CYYU(JMIN)
!  COEFICIENTS FOR (P)YY FROM J=JMIN+1 TO J=JMAX-1
    JSTART = JMIN + 1
    JEND=JMAX-1
    DO 2 J=JSTART,JEND
        DYD=Y(J)-Y(J-1)
        DYU=Y(J+1)-Y(J)
        DYC=Y(J+1)-Y(J-1)
        CYYD(J)=2./(DYD*DYC)
        CYYU(J)=2./(DYU*DYC)
        CYYC(J) = CYYD(J) + CYYU(J)
2   CONTINUE
!                                 COEFFICIENTS FOR (P)YY AT JMAX.
    DYD = Y(JMAX) - Y(JMAX-1)
    CYYD(JMAX) = 2./(DYD*DYD)
    CYYU(JMAX) = 2./DYD
    CYYC(JMAX) = CYYD(JMAX)
!
!  COEFFICIENTS FOR VELOCITY FORMULAS
!
    ISTART = IMIN + 1
    DO 3 I = ISTART,IMAX
        XDIFF(I) = 1./(X(I) - X(I-1))
3   CONTINUE
    JSTART = JMIN +1
    DO 5 J = JSTART , JMAX
        YDIFF(J) = 1./(Y(J) - Y(J-1))
5   CONTINUE
!
! COEFFICIENTS FOR EXTRAPOLATION FORMULAS FOR AIRFOIL SURFACE PROPERTIES
!
    CJLOW  = -Y(JLOW-1) / (Y(JLOW) - Y(JLOW-1))
    CJLOW1 = -Y(JLOW)   / (Y(JLOW) - Y(JLOW-1))
    CJUP   = Y(JUP+1) / (Y(JUP+1) - Y(JUP))
    CJUP1  = Y(JUP)   / (Y(JUP+1) - Y(JUP))
!                  COMPUTE SPECIAL DIFFERENCE COEFFICIENTS FOR PYY
!                  TO USE FOR AIRFOIL BOUNDARY CONDITION
!                  DIFFERENCE COEFFICIENTS FOR UPPER SURFACE
    CYYBUD = -2./(Y(JUP+1) + Y(JUP))
    CYYBUC = -CYYBUD / (Y(JUP+1) - Y(JUP))
    CYYBUU =  CYYBUC
!                  DIFFERENCE COEFFICIENTS FOR LOWER SURFACE
    CYYBLU=-2.0/(Y(JLOW) + Y(JLOW-1))
    CYYBLC = CYYBLU / (Y(JLOW) - Y(JLOW-1))
    CYYBLD = CYYBLC
    RETURN
END
! DECK DRAG
FUNCTION DRAG(CDFACT)
!                  COMPUTES PRESSURE DRAG BY INTEGRATING U*V AROUND
!                  AIRFOIL USING TRAPEZODIAL RULE.
!                  CALLED BY - CDCOLE.
!
!ALL BLANK
    COMMON         P(102,101),X(100)  , Y(100)
!ALL COM1
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
!ALL COM6
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
!ALL COM7
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
    COMMON /COM30/ XI(100)  , ARG(100)  , REST(204)
!
    K = 1
    ARG(1) = 0.
    XI(1) = X(ILE-1)
    DO 10 I = ILE,ITE
        K = K+1
        PXUP = CJUP*PX(I,JUP) - CJUP1*PX(I,JUP+1)
        PXLOW = CJLOW*PX(I,JLOW) - CJLOW1*PX(I,JLOW-1)
        ARG(K) = FXU(K-1)*PXUP - FXL(K-1)*PXLOW
        XI(K) = X(I)
10  CONTINUE
    K = K + 1
    ARG(K) = 0.
    XI(K) = X(ITE+1)
    CALL TRAP(XI,ARG,K,SUM)
    DRAG = - SUM*CDFACT*2.
    RETURN
END

SUBROUTINE DROOTS
!                  COMPUTE CONSTANTS ALPHA0,APLHA1,ALPHA2,OMEGA0,OMEGA1
!                  OMEGA2 USED IN FORMULA FOR DOUBLET IN SLOTTED WIND
!                  TUNNEL WITH SUBSONIC FREESTREAM
!                  CALLED BY - FARFLD.
!
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    REAL JET
    COMMON /COM16/ ALPHA0   , ALPHA1  , ALPHA2  , XSING   ,&
    &OMEGA0   , OMEGA1  , OMEGA2  , JET
!
    ERROR = .00001
!                  COMPUTE ALPHA0
    ALPHA0 = 0.
    DO 10 I = 1,100
        TEMP = ALPHA0
        Q = F*TEMP - RTKPOR
        ALPHA0 = HALFPI - ATAN(Q)
        DALPHA = ABS(ALPHA0 - TEMP)
        IF(DALPHA .LT. ERROR) GO TO 15
10  CONTINUE
    N = 0
    GO TO 9999
15  CONTINUE
!                  COMPUTE ALPHA1
    ALPHA1 = 0.
    DO 20 I = 1,100
        TEMP = ALPHA1
        Q = F*(TEMP - PI) - RTKPOR
        ALPHA1 = HALFPI - ATAN(Q)
        DALPHA = ABS(ALPHA1 - TEMP)
        IF(DALPHA .LT. ERROR) GO TO 25
20  CONTINUE
    N = 1
    GO TO 9999
25  CONTINUE
!                  COMPUTE ALPHA2
    ALPHA2 = 0.
    DO 30 I=1,100
        TEMP = ALPHA2
        Q = F*(TEMP - TWOPI) - RTKPOR
        ALPHA2 = HALFPI - ATAN(Q)
        DALPHA = ABS(ALPHA2 - TEMP)
        IF(DALPHA .LT. ERROR) GO TO  35
30  CONTINUE
    N = 2
    GO TO 9999
35  CONTINUE
!                  COMPUTE OMEGA0,OMEGA1,OMEGA2
    TEMP = 1.0 / TAN(ALPHA0)
    OMEGA0 = 1./(1. + F/(1.+ TEMP*TEMP))
    TEMP = 1.0 / TAN(ALPHA1)
    OMEGA1 = 1./(1. + F/(1. + TEMP*TEMP))
    TEMP = 1.0 / TAN(ALPHA2)
    OMEGA2 = 1./(1. + F/(1. + TEMP*TEMP))
    RETURN
!                  ABNORMAL STOP IF ITERATION FOR ALPHAS NOT CONVERGED
9999 CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1000) N
    WRITE(15,1000) N
1000 FORMAT(35H1ABNORMAL STOP IN SUBROUTINE DROOTS/&
    &38H0NONCONVERGENCE OF ITERATION FOR ALPHA, I1)
    STOP
END

SUBROUTINE ECHINP
!                  PRINTS INPUT CARDS USED FOR RUN.
!                  CALLED BY - TSFOIL.
    DIMENSION   CRD(20)
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,110)
    WRITE (15,110)
10  CONTINUE
    READ (5,120,END=30) CRD
!                YES, NO
20  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,100) CRD
    WRITE (15,100) CRD
    GO TO 10
30  CONTINUE
    REWIND 5
    RETURN
!
100 FORMAT(1H ,20A4)
110 FORMAT(1H1)
120 FORMAT(20A4)
!
END

FUNCTION EMACH1(U)
!                  FUNCTION EMACH1 COMPUTES LOCAL SIMILARTY PARAMETER
!                  OR LOCAL MACH NUMBER
!                  CALLED BY - MACHMP, PRINT1, PRTFLD.
!
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
!
!                  COMPUTE SIMILARITY PARAMETER BASED ON LOCAL VELOCITY
    AK1 = AK - GAM1*U
    IF(PHYS) GO TO 5
!                  RETURN VALUE OF LOCAL SIMILARITY PARAMETER
    EMACH1 = AK1
    RETURN
5   CONTINUE
!                  COMPUTE VALUE OF LOCAL MACH NUMBER AND RETURN
!                  COLE SCALING
    ARG = DELRT2*AK1
!                  SPREITER SCALING
    IF(SIMDEF .EQ. 2) ARG = ARG*EMROOT*EMROOT
!                  KRUPP SCALING
    IF(SIMDEF .EQ. 3)  ARG = ARG*EMACH
    ARG = 1. - ARG
    EMACH1 = 0.
    IF(ARG .GT. 0.)EMACH1 = SQRT(ARG)
    RETURN
END

SUBROUTINE EXTRAP(XP,YP,PNEW)
!                  COMPUTE P AT X, YP USING FAR FIELD SOLUTION
!                  FOR SUBSONIC FLOW
!                  CALLED BY - GUESSP.
!
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    COMMON /COM15/ B        , BETA0   , BETA1   , BETA2   , PSI0    ,&
    &PSI1     , PSI2
    REAL JET
    COMMON /COM16/ ALPHA0   , ALPHA1  , ALPHA2  , XSING   ,&
    &OMEGA0   , OMEGA1  , OMEGA2  , JET
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
    IF(BCTYPE .NE. 1) GO TO 100
!                  FREE AIR BOUNDARY CONDITION
    IF(ABS(YP) .LT. 1.E-6)  YP = -1.E-6
    XI = XP - XSING
    ETA = YP*RTK
    PNEW = - CIRCFF/TWOPI*(ATAN2(ETA,XI) + PI - SIGN(PI,ETA))&
    &+ DUB /TWOPI/RTK*(XI/(XI*XI + ETA*ETA))
    RETURN
100 CONTINUE
!                  TUNNEL WALL BOUNDARY CONDITION
    ETA = YP/H
    XI = (XP - XSING)/(H*RTK)
    IF(XI .LT. 0.) GO TO 200
!                  XP IS DOWNSTREAM OF AIRFOIL
    TERM = ETA
    IF(BCTYPE .NE. 3)  TERM = SIN(ETA*BETA0)/BETA0
    PNEW = - .5*CIRCFF*(1. - SIGN(1.,ETA)&
    &+ (1.-JET)*PSI0*TERM*EXP(-BETA0*XI))&
    &+ DUB *.5/(AK*H)*(B + OMEGA0*COS(ETA*ALPHA0)&
    &*EXP(-ALPHA0*XI))
    RETURN
200 CONTINUE
!                  XP IS UPSTREAM OF AIRFOIL
    TERM = 0.
    IF(JET .NE. 0.) TERM = JET*ETA/(1.+F)
    ARG1 = PI - ALPHA1
    ARG2 = PI - BETA2
    PNEW = -.5*CIRCFF*(1.-TERM-PSI2*SIN(ETA*ARG2)/ARG2*EXP(ARG2*XI))&
    &-.5*DUB /(AK*H)*((1.-B)*OMEGA1*COS(ETA*ARG1)*EXP(XI*ARG1))
    RETURN
END

SUBROUTINE FARFLD
!                  SUBROUTINE COMPUTES BOUNDARY DATA FOR OUTER
!                  BOUNDARIES.
!                  CALLED BY - TSFOIL.
!
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    COMMON /COM15/ B        , BETA0   , BETA1   , BETA2   , PSI0    ,&
    &PSI1     , PSI2
    REAL JET
    COMMON /COM16/ ALPHA0   , ALPHA1  , ALPHA2  , XSING   ,&
    &OMEGA0   , OMEGA1  , OMEGA2  , JET
    COMMON /COM24/ DTOP(100), DBOT(100),DUP(100), DDOWN(100),&
    &VTOP(100), VBOT(100),VUP(100), VDOWN(100)
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
!                  TEST FOR SUPERSONIC OR SUBSONIC FREESTREAM
    IF(AK .GT. 0.) GO TO 99
    IF (F .NE. 0.0 .AND. H .NE. 0.0) GO TO 10
    FHINV = 1.0
    GO TO 99
10  CONTINUE
    FHINV = 1.0 / (F * H)
!                  SUPERSONIC FREESTREAM
!                  UPSTREAM BOUNDARY CONDITIONS CORRESPOND TO UNIFORM
!                  UNDISTURBED FLOW
!                  DOWNSTREAM BOUNDARY REQUIRED TO BE SUPERSONIC
!                  TOP AND BOTTOM BOUNDARIES USE SIMPLE WAVE SOLUTION.
    RETURN
!
99  CONTINUE
!
!                  SUBSONIC FREESTREAM
!
!                  FUNCTIONAL FORM OF THE POTENTIAL ON OUTER BOUNDARIES
!                  IS PRESCRIBED. EQUATIONS REPRESENT ASYMPTOTIC FORM
!                  FOR DOUBLET AND VORTEX IN FREE AIR AND WIND TUNNEL
!                  ENVIRONMENT. DOUBLET AND VORTEX ARE LOCATED AT
!                  X=XSING, Y=0.
!                  ACTUAL BOUNDARY VALUES ARE SET IN SUBROUTINES RECIRC
!                  AND REDUB WHERE THE FUNCTIONAL FORMS ARE MULTIPLIED
!                  BY THE VORTEX AND DOUBLET STRENGTHS.  THE BOUNDARY
!                  CONDITIONS ARE CALCULATED HERIN FOR THE INPUT X AND
!                  Y MESH AND VALUES ARE DELETED FOR THE COARSE MESH IN
!                  SUBROUTINE SETBC.
!
!                  SET LOCATION OF SINGLULAR VORTEX AND DOUBLET
    XSING = .5
!                  SET DEFAULT VALUES FOR TUNNEL WALL PARAMETERS
    B = 0.
    OMEGA0 = 1.
    OMEGA1 = 1.
    OMEGA2 = 1.
    JET = 0.
    PSI0 = 1.
    PSI1 = 1.
    PSI2 = 1.
!                  BRANCH TO APPROPRIATE FORMULAS DEPENDING ON BCTYPE
    GO  TO (100,200,300,400,500,600),BCTYPE
100 CONTINUE
!                  BCTYPE = 1
!                  FREE AIR BOUNDARY CONDITION
!                  SET BOUNDARY ORDINATES
    YT = YIN(JMAX)*RTK
    YB = YIN(JMIN)*RTK
    XU = XIN(IMIN) - XSING
    XD = XIN(IMAX) - XSING
    YT2 = YT*YT
    YB2 = YB*YB
    XU2 = XU*XU
    XD2 = XD*XD
    COEF1 = 1./TWOPI
    COEF2 = 1.0/(TWOPI*RTK)
!                  COMPUTE DOUBLET AND VORTEX TERMS ON TOP AND BOTTOM
!                  BOUNDARIES.
    DO 110 I=IMIN,IMAX
        XP = XIN(I) - XSING
        XP2 = XP*XP
        DTOP(I) = XP/(XP2 + YT2)*COEF2
        DBOT(I) = XP/(XP2 + YB2)*COEF2
        VTOP(I) = -ATAN2(YT,XP)*COEF1
        VBOT(I)  = -(ATAN2(YB,XP) + TWOPI)*COEF1
110 CONTINUE
!                  COMPUTE DOUBLET AND VORTEX TERMS ON UPSTREAM
!                  AND DOWNSTREAM BOUNDARIES
    DO 120 J=JMIN,JMAX
        YJ = YIN(J)*RTK
        YJ2 = YJ*YJ
        DUP(J) = XU/(XU2 + YJ2)*COEF2
        DDOWN(J) = XD/(XD2 + YJ2)*COEF2
        Q = PI - SIGN(PI,YJ)
        VUP(J) = -(ATAN2(YJ,XU) + Q)*COEF1
        VDOWN(J) = -(ATAN2(YJ,XD) + Q)*COEF1
120 CONTINUE
    IF (AK .GT. 0.0) CALL ANGLE
    RETURN
!                  COMPUTE WALL CONSTANTS FOR VARIOUS WIND TUNNEL
!                  CONDITIONS.
200 CONTINUE
!                  BCTYPE = 2
!                  SOLID WALL TUNNEL
    POR = 0.
!                  SET CONSTANTS FOR DOUBLET SOLUTION
    B = .5
    ALPHA0 = PI
    ALPHA1 = PI
    ALPHA2 = PI
!                  SET CONSTANTS FOR VORTEX SOLUTION
    BETA0 = HALFPI
    BETA1 = HALFPI
    BETA2 = HALFPI
    GO TO 700
300 CONTINUE
!                  BCTYPE = 3
!                  FREE JET
    F = 0.
    RTKPOR = 0.
!                  SET CONSTANTS FOR DOUBLET SOLUTION
    ALPHA0 = HALFPI
    ALPHA1 = HALFPI
    ALPHA2 = HALFPI
!                  SET CONSTANTS FOR VORTEX SOLUTION
    JET = .5
    BETA0 = 0.
    BETA1 = 0.
    BETA2 = 0.
    GO TO 700
400 CONTINUE
!                  BCTYPE = 4
!                  IDEAL SLOTTED WALL
    RTKPOR = 0.
    FHINV = 1.0 / ( F * H )
!                  SET CONSTANTS FOR DOUBLET SOLUTION
    CALL DROOTS
!                  SET CONSTANTS FOR VORTEX SOLUTION
    JET = .5
    CALL VROOTS
    GO TO 700
500 CONTINUE
!                  BCTYPE = 5
!                  IDEAL PERFORATED/POROUS WALL
    F = 0.
    RTKPOR = RTK/POR
!                  SET CONSTANTS FOR DOUBLET SOLUTION
    ALPHA0 = HALFPI - ATAN(-RTKPOR)
    ALPHA1 = ALPHA0
    ALPHA2 = ALPHA0
!                  SET CONSTANTS FOR VORTEX SOLUTION
    BETA0 = ATAN(RTKPOR)
    BETA1 = BETA0
    BETA2 = BETA1
    GO TO 700
600 CONTINUE
!                  BCTYPE = 6
!                  GENERAL HOMOGENEOUS WALL BOUNDARY CONDITION
!                  BOUNDARY CONDITION IS NOT OPERABLE YET IN FINITE
!                  DIFFERENCE SUBROUTINES.
!                  FAR FIELD SOLUTION HAS BEEN DERIVED AND IS INCLUDED
!                  HERE  FOR FUTURE USE
    RTKPOR = RTK/POR
    CALL DROOTS
    CALL VROOTS
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1000)
    WRITE(15,1000)
1000 FORMAT(35H1ABNORMAL STOP IN SUBROUTINE FARFLD/&
    &24H BCTYPE=6 IS NOT USEABLE)
    STOP
700 CONTINUE
!                  COMPUTE FUNCTIONAL FORMS FOR UPSTREAM AND DOWNSTREAM
!                  BOUNDARY CONDITIONS  FOR DOUBLET AND VORTEX
    XU = (XIN(IMIN) - XSING)/(RTK*H)
    XD = (XIN(IMAX) - XSING)/(RTK*H)
!                  DOUBLET TERMS
    COEF1 = .5/AK/H
    ARG0 = ALPHA0
    ARG1 = PI - ALPHA1
    ARG2 = TWOPI - ALPHA2
    EXARG0 = EXP(-ARG0*XD)
    EXARG1 = EXP(ARG1*XU)
    EXARG2 = EXP(ARG2*XU)
    DO 720 J = JMIN,JMAX
        YJ = YIN(J) / H
        DDOWN(J) = COEF1*(B + OMEGA0*COS(YJ*ARG0)*EXARG0)
        DUP(J) = -COEF1*((1.-B)*OMEGA1*COS(YJ*ARG1)*EXARG1 +&
        &OMEGA2*COS(YJ*ARG2)*EXARG2)
720 CONTINUE
!                  VORTEX TERMS
    ARG0 = BETA0
    ARG1 = PI + BETA1
    ARG2 = PI - BETA2
    EXARG0 = EXP(-ARG0*XD)
    EXARG1 = EXP(-ARG1*XD)
    EXARG2 = EXP(ARG2*XU)
    DO 740 J = JMIN,JMAX
        YJ = YIN(J) / H
        TERM = YJ
        IF (JET .EQ. 0.0) TERM = SIN(YJ*ARG0) / ARG0
        VDOWN(J) = -.5*(1. - SIGN(1.,YJ) + (1. - JET)*PSI0*TERM*EXARG0 +&
        &PSI1*SIN(YJ*ARG1)*EXARG1/ARG1)
        TERM = 0.
        IF (JET .NE. 0.0) TERM = JET * YJ / (1.0 + F)
        VUP(J) = -.5*(1. - TERM - PSI2*SIN(YJ*ARG2)*EXARG2/ARG2)
740 CONTINUE
    RETURN
END

SUBROUTINE FINDSK(ISTART,IEND,J, ISK)
!                  SUBROUTINE LOCATES SHOCK WAVE ON LINE J BETWEEN
!                  ISTART AND IEND.  SHOCK IS LOCATED AT SHOCK POINT.
!                  IF NO SHOCK FOUND, ISK IS SET NEGATIVE.
!                  CALLED BY - CDCOLE.
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    ISK = ISTART - 1
    U2 = PX(ISK,J)
5   ISK = ISK + 1
    U1 = U2
    U2 = PX(ISK,J)
    IF(U1 .GT. SONVEL .AND. U2 .LE. SONVEL) GO TO 10
    IF(ISK .LT. IEND) GO TO 5
    ISK = - IEND
10  CONTINUE
    RETURN
END

SUBROUTINE FIXPLT
!
!                        SETS UP ARRAYS FOR SUBROUTINE CPPLOT.
!                        CALLED BY - PRINT.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    COMMON /COM25/ CPL(100) , CPU(100) , IDLA
    COMMON /COM30/ CPUP(101), CPLO(101) , CPS(101) , XP(101)
!
    YMX = 5.*CPFACT
    YMN = -5.*CPFACT
    K = 0
    DO 150 I = IMIN,IMAX
        K = K + 1
        QCP = -CPU(I)
        QCP = AMAX1(QCP,YMN)
        QCP = AMIN1(QCP,YMX)
        CPUP(K) = QCP
        QC1 = -CPL(I)
        QC1 = AMAX1(QC1,YMN)
        QC1 = AMIN1(QC1,YMX)
        CPLO(K) = QC1
        QC2 = -CPSTAR
        QC2 = AMAX1(QC2,YMN)
        QC2 = AMIN1(QC2,YMX)
        CPS(K) = QC2
        XP(K) = X(I)
150 CONTINUE
    IMP = K + 1
    CPUP(IMP) = YMX
    CPLO(IMP) = YMN
    CPS(IMP)  = 0.0
    XP(IMP) = X(IMAX) + .001
    CALL CPPLOT (XP, CPUP, CPLO, CPS, IMP)
    RETURN
END

SUBROUTINE GUESSP
!                  SUBROUTINE INITIALIZES P ARRAY AS FOLLOWS
!                       PSTART = 1  P SET TO ZERO
!                       PSTART = 2  P READ FROM UNIT 7
!                       PSTART = 3  P SET TO VALUES IN CORE
!                  IF PSTART = 2 OR 3, SOLUTION IS INTERPOLATED FROM
!                  XOLD, YOLD TO X,Y
!                  BOUNDARY CONDITIONS FOR P ON OUTER BOUNDARIES ARE
!                  AUTOMATICALLY SET DURING INITIALIZATION
!                  CALLED BY - TSFOIL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    INTEGER        PSTART
    LOGICAL        PSAVE
    COMMON /COM11/ ALPHAO   , CLOLD   , DELTAO  , DUBO    , EMACHO  ,&
    &IMINO    , IMAXO   , IMAXI   , JMINO   , JMAXO   ,&
    &JMAXI    , PSAVE   , PSTART  ,TITLE(20),TITLEO(20),&
    &VOLO     , XOLD(100),YOLD(100)
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM30/ PT(100)  , REST(304)
!
!                  BRANCH TO APPROPRIATE LOCATION
    GO TO ( 200, 300, 400) , PSTART
200 CONTINUE
!                  PSTART = 1
!                  P SET TO ZERO
    DO 205 I=1,101
        DO 205 J=1,102
            P(J,I) = 0.
205 CONTINUE
    DUB = 0.
    CIRCFF = 0.
    CIRCTE=0.0
    RETURN
300 CONTINUE
!                  PSTART = 2
!                  P, X, Y ARRAYS READ FROM UNIT 7 IN SUBROUTINE READIN
!                  TOGETHER WITH INFORMATION ABOUT OLD SOLUTION
400 CONTINUE
!                  PSTART = 3
!                  ARRAYS FROM PREVIOUS CASE ARE ALREADY IN P,XOLD,YOLD
    DUB = DUBO
    CIRCFF = CLOLD/CLFACT
    CIRCTE=CIRCFF
!
!                  FOR PSTART = 2 OR 3, OLD P ARRAY ON XOLD, YOLD MESH
!                  MUST BE INTERPOLATED ONTO NEW X Y MESH.
!
!                  INTERPOLATE P FROM XOLD,YOLD TO X,YOLD
!                  CHECK TO SEE IF XOLD AND XIN ARE THE SAME MESH
    IF(IMAXI .NE. IMAXO) GO TO 450
    DO 410 I=IMIN, IMAXI
        TEST = ABS(XIN(I) - XOLD(I))
        IF(TEST .GT. .0001) GO TO 450
410 CONTINUE
!                  XIN AND XOLD ARE SAME MESH.
!                  P ARRAY MAY BE INTERPOLATED BY SIMPLE DELETION OF
!                  VALUES AT MESH POINTS DELETED IN SUBROUTINE CUTOUT
!                  IF IREF .LE. ZERO, NO INTERPOLATION IS NEEDED
    IF(IREF .LE. 0) GO TO 500
    ISTEP = 2*IREF
    DO 430 J = JMINO,JMAXO
        INEW = 0
        DO 420 I = IMINO,IMAXO,ISTEP
            INEW = INEW + 1
            P(J,INEW) = P(J,I)
420     CONTINUE
430 CONTINUE
!                  INTERPOLATION IN X DIRECTION COMPLETE IF XIN AND
!                  XOLD ARE THE SAME.
    GO TO 500
450 CONTINUE
!                  INTERPOLATE FROM XOLD TO X FOR ARBITRARY CASE
    DO 490 J = JMINO, JMAXO
        YP = YOLD(J)
        DO 475 I = IMIN, IMAX
            XP = X(I)
            IF(XP .LT. XOLD(IMINO)) GO TO 470
            IF(XP .GT. XOLD(IMAXO)) GO TO 470
!                  NEW X MESH POINT WITHIN RANGE OF OLD X MESH
!                  FIND VALUE OF XOLD .GT. XP
            X2 = XOLD(1)
            K = 0
455         K = K +1
            X1 = X2
            X2 = XOLD(K)
            IF (X2 .LT. XP) GO TO 455
            IF (X2 .GT. XP) GO TO 460
            PT(I) = P(J,K)
            GO TO 475
460         CONTINUE
            P1 = P(J,K-1)
            P2 = P(J,K)
            PT(I) = P1 + (P2 - P1)/(X2 - X1) * (XP - X1)
            GO TO 475
470         CONTINUE
!                  NEW X MESH POINT IS OUTSIDE RANGE OF OLD X MESH
!                  FOR SUPERSONIC FREESTREAM SET P=0, FOR SUBSONIC
!                  FREESTREAM, EXTRAPOLATE USING FAR FIELD SOLUTION
            PT(I) = 0.
            IF(AK .GT. 0.) CALL EXTRAP(XP,YP,PT(I))
475     CONTINUE
!                  WRITE NEW VALUES FOR P INTO P ARRAY
        DO 480 I = IMIN, IMAX
            P(J,I) = PT(I)
480     CONTINUE
490 CONTINUE
500 CONTINUE
!                  INTERPOLATE FROM X,YOLD TO X,Y
!                  CHECK TO SEE IF YIN AND YOLD ARE THE SAME MESH
!
    IF(JMAXI .NE. JMAXO) GO TO  550
    DO 510 J = JMIN, JMAXI
        TEST = ABS(YIN(J) - YOLD(J))
        IF(TEST .GT. .0001) GO TO 550
510 CONTINUE
!                  YIN AND YOLD ARE THE SAME MESH
!                  P ARRAY MAY BE INTERPOLATED BY SIMPLE DELETION OF
!                  VALUES AT MESH POINTS DELETED IN SUBROUTINE CUTOUT
!                  IF IREF .LE. ZERO, NO INTERPOLATION IS NEEDED
    IF(IREF .LE. 0) GO TO 600
    JSTEP = 2*IREF
    DO 530 I= IMIN, IMAX
        JNEW = 0
        DO 520 J = JMINO, JMAXO, JSTEP
            JNEW = JNEW + 1
            P(JNEW,I) = P(J,I)
520     CONTINUE
530 CONTINUE
!                  INTERPOLATION IN Y DIRECTION COMPLETE IF YIN AND
!                  YOLD ARE THE SAME.
    GO TO 600
550 CONTINUE
!                  INTERPOLATE YOLD TO Y FOR ARBITRARY CASE
    DO 590 I = IMIN,IMAX
        XP = X(I)
        K = 2
        Y1 = YOLD(1)
        DO 575 J = JMIN,JMAX
            YP = Y(J)
            IF(YP .LT. YOLD(JMINO)) GO TO 570
            IF(YP .GT. YOLD(JMAXO)) GO TO 571
!                  NEW Y MESH POINT WITHIN RANGE OF OLD Y MESH
!                  FIND VALUE OF YOLD .GT. YP
            Y2 = Y1
            K = K - 1
555         K = K + 1
            Y1 = Y2
            Y2 = YOLD(K)
            IF(Y2 .LE. YP) GO TO 555
            P1 = P(K-1,I)
            P2 = P(K,I)
            PT(J) = P1 + (P2 - P1) / (Y2 - Y1) * (YP - Y1)
            GO TO 575
!                  NEW Y MESH POINT OUTSIDE RANGE OF OLD Y MESH.
!                  SET P(J,I) = P(JMINO,I) OR, IF SUBSONIC FREESTREAM
!                  FREE AIRFLOW, EXTRAPOLATE FOR P(J,I) USING FARFLD
!                  FORMULA.
570         CONTINUE
            PT(J) = P(JMINO,I)
            GO TO 572
571         PT(J) = P(JMAXO,I)
572         CONTINUE
            IF(AK .GT. 0. .AND. BCTYPE .EQ. 1) CALL EXTRAP(XP,YP,PT(J))
575     CONTINUE
!                  PUT NEW P VALUES INTO P ARRAY
        DO 580 J = JMIN,JMAX
            P(J,I) = PT(J)
580     CONTINUE
590 CONTINUE
600 CONTINUE
    RETURN
END

SUBROUTINE INPERR (I)
!
!                        FATAL ERROR IN INPUT. WRITE MESSAGE AND STOP.
!                        CALLED BY - READIN, SCALE.
!
!     Changed to write to a file - Andy Ko 4/3/03
!      IF (I .EQ. 1) WRITE (6,901)
!      IF (I .EQ. 2) WRITE (6,902)
!      IF (I .EQ. 3) WRITE (6,903)
!      IF (I .EQ. 4) WRITE (6,904)
!      IF (I .EQ. 5) WRITE (6,905)
!      IF (I .EQ. 6) WRITE (6,906)
!      IF (I .EQ. 7) WRITE (6,907)
!      IF (I .EQ. 8) WRITE(6,908)

    IF (I .EQ. 1) WRITE (15,901)
    IF (I .EQ. 2) WRITE (15,902)
    IF (I .EQ. 3) WRITE (15,903)
    IF (I .EQ. 4) WRITE (15,904)
    IF (I .EQ. 5) WRITE (15,905)
    IF (I .EQ. 6) WRITE (15,906)
    IF (I .EQ. 7) WRITE (15,907)
    IF (I .EQ. 8) WRITE (15,908)
    STOP
901 FORMAT(1H0,5X,45HIMAX OR JMAX IS GREATER THAN 100,NOT ALLOWED.)
902 FORMAT(1H0,5X,39HX MESH POINTS NOT MONOTONIC INCREASING.)
903 FORMAT(1H0,5X,39HY MESH POINTS NOT MONOTONIC INCREASING.)
904 FORMAT(1H0,5X,44HMACH NUMBER NOT IN PERMITTED RANGE. (.5,2.0))
905 FORMAT(1H0,5X,41HALPHA NOT IN PERMITTED RANGE. (-9.0, 9.0))
906 FORMAT(1H0,5X,41HDELTA NOT IN PERMITTED RANGE. ( 0.0, 1.0))
907 FORMAT(1H0,5X,45HAK=0. VALUE OF AK MUST BE INPUT SINCE PHYS=F.)
908 FORMAT(1H0,5X,60HMACH NUMBER IS NOT LESS THAN 1.0 FOR VISCOUS WEDG&
    &E INCLUSION )
END

SUBROUTINE ISLIT ( X )
!
!                        ISLIT COMPUTES ILE AND ITE FOR ANY XMESH ARRAY.
!                        CALLED BY - CKMESH, CUTOUT, READIN, REFINE.
!
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
!
    DIMENSION X(100)
    I = IMIN - 1
10  CONTINUE
    I = I + 1
    IF (X(I) .LT. 0.0) GO TO 10
    ILE = I
20  CONTINUE
    I = I + 1
    IF (X(I) .LE. 1.0) GO TO 20
    ITE = I - 1
    RETURN
END

SUBROUTINE JSLIT ( Y )
!
!                        JSLIT COMPUTES JLOW AND JUP FROM Y ARRAY.
!                        CALLED BY - CKMESH, CUTOUT, READIN, REFINE.
!
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
!
    DIMENSION  Y(100)
    J = JMIN - 1
10  CONTINUE
    J = J + 1
    IF (Y(J) .LT. 0.0) GO TO 10
    JLOW = J - 1
    JUP  = J
    RETURN
END
REAL FUNCTION LIFT (CLFACT)
!                  COMPUTE LIFT COEFFICIENT FROM JUMP IN P AT TRAILING
!                  EDGE OF AIRFOIL
!                  CALLED BY - PRINT1, SOLVE.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
!
    PTOP = CJUP*P(JUP,ITE) - CJUP1*P(JUP + 1 , ITE)
    PBOT = CJLOW*P(JLOW,ITE) - CJLOW1*P(JLOW-1,ITE)
    LIFT = 2.*CLFACT*(PTOP-PBOT)
    RETURN
END

SUBROUTINE MACHMP
!
!                        SUBROUTINE TO PRINT MAP OF MACH NO. ROUNDED
!                        TO NEAREST .1. CALLED BY - PRINT.
!
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON /COM30/ MM(100) , REST(304)
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
!
    DATA IB/1H / , IP/1H+/ , IM/1H-/ , IL/1HL/ , IT/1HT/
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,400)
    WRITE (15,400)
    DO 60 K=2,JMAX
        J        = JMAX - K + 2
        IJC = IB
        IF ( J .EQ. JUP) IJC = IP
        IF ( J .EQ. JLOW ) IJC = IM
        DO 10 I=1,IMAX
            MM(I)    = IB
10      CONTINUE
        DO 50 I=2,IMAX
            U        = PX(I,J)
            EM       = EMACH1 (U)
            WRITE (18,'(F16.12)', ADVANCE="NO") EM
            WRITE (21,'(F16.12)', ADVANCE="NO") -2. * U * CPFACT
            IF (EM .GT. 0.0) GO TO 20
            MM(I)    = 0
            GO TO 40
20          CONTINUE
            IF (EM .LE. 1.05) GO TO 30
            EM       = EM - 1.0
            GO TO 20
30          CONTINUE
            MM(I)    = INT(10. * EM + .5)
40          CONTINUE
50      CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,410) IJC, (MM(I),I=2,IMAX)
        WRITE (18,'(A)') ""
        WRITE (21,'(A)') ""
        WRITE (15,410) IJC, (MM(I),I=2,IMAX)
60  CONTINUE
    DO 70 I=1,IMAX
        MM(I) = IB
        IF (I .EQ. ILE) MM(I) = IL
        IF (I .EQ. ITE) MM(I) = IT
70  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,420) (MM(I),I=2,IMAX)
    WRITE (15,420) (MM(I),I=2,IMAX)
    RETURN
!
400 FORMAT(40H1  MACH NO. MAP.   ROUNDED TO NEAREST .1///)
410 FORMAT (11X,A1,99I1)
420 FORMAT (12X,99A1)
!
END

SUBROUTINE M1LINE
!                  PRINTS COORDINATES WHERE SONIC VELOCITY IS COMPUTED
!                  LINEAR INTERPOLATION BETWEEN MESH POINTS IS USED
!                  CALLED BY - PRINT.
! **********************************************************************
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM30/ XSLPRT(200) , YSLPRT(200) , REST(4)
!
    DIMENSION  XSONIC(10)
    NPTS = 0
    KMIN = JMIN
    KMAX = JMAX
    JP = JMAX + JMIN
    DO 200 K=KMIN,KMAX
        J = JP - K
        YPR = YFACT*Y(J)
        PX2 = PX(IMIN,J)
        M = 0
        IF (J .NE. JLOW) GO TO 170
!     Changed to write to a file - Andy Ko 4/3/03
!      IF (NPTS .NE. 0) WRITE (6,2070)
        IF (NPTS .NE. 0) WRITE (15,2070)
170     CONTINUE
        IMM = IMIN + 1
        DO 180 I=IMM,IMAX
            PX1 = PX2
            PX2 = PX(I,J)
            IF(PX1 .GT. SONVEL .AND. PX2 .GT. SONVEL) GO TO 180
            IF(PX1 .LT. SONVEL .AND. PX2 .LT. SONVEL) GO TO 180
!     Changed to write to a file - Andy Ko 4/3/03
!      IF (NPTS .EQ. 0) WRITE (6,2000)
            IF (NPTS .EQ. 0) WRITE (15,2000)
            M = M+1
            RATIO = (SONVEL-PX1)/(PX2-PX1)
            XSONIC(M) = X(I-1) + (X(I)-X(I-1))*RATIO
            NPTS = NPTS  + 1
            XSLPRT(NPTS) = XSONIC(M)
            YSLPRT(NPTS) = YPR
            IF(NPTS .GE. 200) GO TO 250
180     CONTINUE
        IF(M .EQ. 0) GO TO 200
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,2040) YPR,(XSONIC(L),L=1,M)
        WRITE(15,2040) YPR,(XSONIC(L),L=1,M)
200 CONTINUE
    GO TO 300
250 CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,2060)
    WRITE(15,2060)
    GO TO 400
300 CONTINUE
    IF(NPTS .EQ.0) GO TO 400
    YM = Y(JMIN)
    YX = Y(JMAX)
    DO 320 N=1,NPTS
        IF (YSLPRT(N) .NE. YM .AND. YSLPRT(N) .NE. YX) GO TO 320
!     Changed to write to a file - Andy Ko 4/3/03
!      IF ( AK .GT. 0.0 ) WRITE (6,2050)
!      IF ( AK .LT. 0.0 .AND. BCTYPE .EQ. 1 ) WRITE (6,2050)
        IF ( AK .GT. 0.0 ) WRITE (15,2050)
        IF ( AK .LT. 0.0 .AND. BCTYPE .EQ. 1 ) WRITE (15,2050)

320 CONTINUE
    XMIN  = -.75
    XMAX  = 1.75
    XINCR =  .25
    YMIN  = -1.0
    YMAX  =  1.5
    YINCR =   .5
    CALL PLTSON(XSLPRT,YSLPRT,XMIN,XMAX,XINCR,YMIN,YMAX,YINCR,NPTS)
400 CONTINUE
    RETURN
!
2000 FORMAT(23H1SONIC LINE COORDINATES/6X,1HY,10X,6HXSONIC//)
2040 FORMAT(11F10.5)
2050 FORMAT(20H0***** CAUTION *****/&
    &34H SONIC LINE HAS REACHED A BOUNDARY/&
    &61H THIS VIOLATES ASSUMPTIONS USED TO DERIVE BOUNDARY CONDITIONS/&
    &29H SOLUTION IS PROBABLY INVALID)
2060 FORMAT(20H0***** CAUTION *****/36H NUMBER OF SONIC POINTS EXDEEDED&
    &200/25H ARRAY DIMENSION EXCEEDED/42H EXECUTION OF SUBROUTINE M1LI&
    &NE TERMINATED)
2070 FORMAT(2X,13HBODY LOCATION)
!
END

SUBROUTINE NEWISK(ISKOLD,J,ISKNEW)
!                  FIND NEW LOCATION OF SHOCKWAVE (ISKNEW) ON LINE J
!                  GIVEN AN INITIAL GUESS FOR LOCATION (ISKOLD).
!                  SHOCK LOCATION IS DEFINED AS LOCATION OF SHOCK POINT.
!                  IF NO SHOCK IS FOUND, ISKNEW IS SET NEGATIVE.
!                  CALLED BY - CDCOLE.
!
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    I2 = ISKOLD + 2
    ISKNEW = ISKOLD - 3
    U2 = PX(ISKNEW,J)
5   ISKNEW = ISKNEW + 1
    U1 = U2
    U2 = PX(ISKNEW,J)
    IF(U1 .GT. SONVEL .AND. U2 .LE. SONVEL) GO TO 10
    IF(ISKNEW .LT. I2) GO TO 5
!                  NO SHOCK POINT FOUND, TIP OF SHOCK REACHED.
    ISKNEW = - ISKNEW
10  RETURN
END
FUNCTION PITCH(CMFACT)
!                  COMPUTE AIRFOIL PITCHING MOMENT ABOUT X = XM, Y=0
!                  THE INTEGRAL OF(X-XM)(CPUPPER-CPLOWER) IS INTEGRATED
!                  BY PARTS TO GIVE AN INTEGRAL IN THE JUMP IN P PLUS A
!                  CONSTANT EQUAL TO THE JUMP IN P AT X = 1. TIMES THE
!                  MOMENT ARM AT THE TRAILING EDGE
!                  CALLED BY - PRINT1, SOLVE.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
    COMMON /COM30/ XI(100)  , ARG(100)  , REST(204)
!                  SET XM TO QUARTER CHORD
!
    XM = .25
    K = 0
    DO 10 I=ILE,ITE
        K = K + 1
        PTOP = CJUP*P(JUP,I) - CJUP1*P(JUP+1,I)
        PBOT = CJLOW*P(JLOW,I) - CJLOW1*P(JLOW-1,I)
        ARG(K) = PTOP - PBOT
        XI(K) = X(I)
10  CONTINUE
    CALL TRAP(XI,ARG,K,SUM)
    PITCH = CMFACT*((1.-XM)*ARG(K) - SUM) * (-2.)
    RETURN
END

SUBROUTINE PLTSON (X,Y,XAXMIN,XMAX,XINCR,YAXMIN,YMAX,YINCR,NPTS)
!
!                             THIS IS A MODIFIED VERSION OF PRNPLT AND
!                             XMAX, XINCR, YMAX, YINCR MUST BE SUPPLIED.
!                             XMIN, AND YMIN MUST ALSO BE SUPPLIED.
!                       PRINTER PLOT ROUTINE M. S. ITZKOWITZ  MAY,1967
!                       PLOTS THE @NPTS@ POINTS GIVEN BY, X(I),Y(I)
!                       ON A 51 X 101 GRID USING A TOTAL OF 56 LINES ON
!                       THE PRINTER.
!                       IF EITHER INCREMENTAL STEP SIZE IS ZERO, THE
!                       PROGRAM EXITS.
!                       NEITHER OF THE INPUT ARRAYS ARE DESTROYED.
!                        CALLED BY - M1LINE.
!
    DIMENSION  X(NPTS),  Y(NPTS),  IGRID(105),  XAXIS(11)
    DIMENSION IBS(9)
!
    INTEGER  BLANK , DOT , STAR , DASH
    DATA     BLANK , DOT , STAR , DASH / 1H , 1H., 1H*, 1H-/
    DATA      IBS/1HB,1HO,1HD,1HY,1H ,1HS,1HL,1HI,1HT/
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,900)
!      WRITE(6,910)
    WRITE(15,900)
    WRITE(15,910)

    IF (XINCR .EQ. 0. .OR. YINCR .EQ. 0.) GO TO 800
    YRNGE  = YMAX - YAXMIN
    XRNGE  = XMAX - XAXMIN
    YVAL = YRNGE * .02
    XVAL = XRNGE * .01
    JSLT = IFIX( 51.*(YMAX/YRNGE) ) + 1
    OXV = 1.0 / XVAL
    OYV = 1.0 / YVAL
    IZERO  = YMAX * OYV  + 1.5
    JZERO = 103.5 - XMAX * OXV
    IF (JZERO .GT. 103 .OR. JZERO .LT. 4) JZERO = 2
    FIZERO = IZERO
    FJZERO = JZERO
    IGRID(1)     = BLANK
    IGRID(2)     = DOT
    IGRID(104)   = DOT
    IGRID(105)   = BLANK
    FIZER5 = FIZERO + .5
    FJZER5 = FJZERO + .5
!                        NOPW IS THE NUMBER OF PRINT WHEELS USED TO
!                                    SPAN THE BODY LENGTH.
    NOPW = OXV
!                        JS - THE NUMBER OF PRT WHEEL POSITIONS TO BE
!                             FILLED AT EACH END OF THE WORD ?BODY SLIT?
    JS = 0
    IF (NOPW .GT. 9) JS = (NOPW-9) / 2
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,905)
!      WRITE (6,903)
    WRITE (15,905)
    WRITE (15,903)
!                        LOOP TO SET UP ONE LINE EACH TIME THRU.
    DO 30 I=1,51
!                        BLANK THE LINE TO BE PRINTED.
        DO 2 J=3,103
            IGRID(J) = BLANK
2       CONTINUE
!
!                        SEARCH ARRAY FOR POINTS ON THIS LINE.
!
        DO 10 K=1,NPTS
            ITEST = FIZER5 - Y(K) * OYV
            IF (ITEST .NE. I) GO TO 7
            J = FJZER5 + X(K) * OXV
            IF (J .GT. 103) J = 103
            IF (J .LT.   3) J = 3
            IGRID(J) = STAR
7           CONTINUE
            IF (JSLT .EQ. I) GO TO 9
            GO TO 10
9           CONTINUE
!                        WRITE ?BODY SLIT? IF THIS IS THE J0 LINE.
            J = JZERO + 1
            IF (JS .EQ. 0) GO TO 4
            DO 3 L=1,JS
                IF (IGRID(J) .NE. STAR) IGRID(J) = DASH
                J = J + 1
3           CONTINUE
4           CONTINUE
            DO 5 L=1,9
                IF (IGRID(J) .NE. STAR) IGRID(J) = IBS(L)
                J = J + 1
5           CONTINUE
            IF (JS .EQ. 0) GO TO 10
            DO 6 L=1,JS
                IF (IGRID(J) .NE. STAR) IGRID(J) = DASH
                J = J + 1
6           CONTINUE
10      CONTINUE
        IF (MOD(I,10) .EQ. 1) GO TO 13
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,901) IGRID
        WRITE (15,901) IGRID
        GO TO 30
13      CONTINUE
        FI = I - 1
        YAXIS = YMAX - FI * YINCR * .1
        IF (ABS(YAXIS) .LT. YAXMIN) YAXIS = 0.
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,902) YAXIS, (IGRID(J),J=1,105)
        WRITE (15,902) YAXIS, (IGRID(J),J=1,105)
30  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,903)
!      WRITE (6,905)
    WRITE (15,903)
    WRITE (15,905)
    DO 40 M=1,11
        FM = 11 - M
        XAXIS(M) = XMAX - XINCR * FM
        IF (XAXIS(M) .LT. XAXMIN) XAXIS(M) = XAXMIN
40  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,904) XAXIS, NPTS
!      WRITE(6,900)
    WRITE (15,904) XAXIS, NPTS
    WRITE(15,900)
    RETURN
!
!     Changed to write to a file - Andy Ko 4/3/03
!  800 WRITE (6,9800)
800 WRITE (15,9800)
    STOP
!
900 FORMAT(1H1)
901 FORMAT(14X,105A1)
902 FORMAT(1X,F10.1,2X,1H+,105A1,1H+)
903 FORMAT(15X,103(1H.))
904 FORMAT(7X,11(F10.2),2H (,I4,5H PTS) )
905 FORMAT(16X,11(1H+,9X))
910 FORMAT(35X,15HSONIC LINE PLOT,10X,6HY VS X,10X,20H *  FOR SONIC PO&
    &INTS//)
9800 FORMAT(46H1SCALING ERROR IN PRNPLT, EXECUTION TERMINATED )
!
END

SUBROUTINE PRBODY

!                  PRINTS OUT BODY GEOMETRY
!                  IF PHYS = .TRUE.  ALL DIMENSIONS ARE NORMALIZED BY
!                                    AIRFOIL CHORD
!                  IF PHYS = .FALSE. ALL DIMENSIONS EXCEPT X ARE
!                                    NORMALIZED BY CHORD LENGTH AND
!                                    THICKNESS RATIO.
!                  CALLED BY - BODY.
!
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1000)
    WRITE(15,1000)
!                  FIND MAXIMUM THICKNESS AND CAMBER
    THMAX   = 0.
    CAMAX   = 0.
    DO 10 I = 1,IFOIL
        THMAX   = AMAX1(THMAX,THICK(I))
        CAMAX   = AMAX1(CAMAX,CAMBER(I))
10  CONTINUE
    THMAX   = 2.*THMAX
    IF(PHYS) GO TO 100
!                  PRINTOUT IN SIMILARITY VARIABLES
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1001) THMAX
!      WRITE(6,1002) VOL,CAMAX
!      WRITE(6,1003)
!      DO 40 I=1,IFOIL
!      WRITE (6,1005) XFOIL(I),FU(I),FXU(I),FL(I),FXL(I),THICK(I),
!     1               CAMBER(I)

    WRITE(15,1001) THMAX
    WRITE(15,1002) VOL,CAMAX
    WRITE(15,1003)
    DO 40 I=1,IFOIL
        WRITE (15,1005) XFOIL(I),FU(I),FXU(I),FL(I),FXL(I),THICK(I),&
        &CAMBER(I)

40  CONTINUE
    RETURN

100 CONTINUE
!                  PRINTOUT IN PHYSICAL VARIALBES

    THMAX    = DELTA*THMAX
    CAMAX    = DELTA*CAMAX
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1006) THMAX
!      VOLUME   = VOL*DELTA
!      WRITE(6,1002) VOLUME,CAMAX
!      WRITE(6,1004)

    WRITE(15,1006) THMAX
    VOLUME   = VOL*DELTA
    WRITE(15,1002) VOLUME,CAMAX
    WRITE(15,1004)

    DO 110 I = 1,IFOIL
        YUP      = DELTA*FU(I)
        YXUP     = DELTA*FXU(I)
        YLO      = DELTA*FL(I)
        YXLO     = DELTA*FXL(I)
        TH       = DELTA*THICK(I)
        CA       = DELTA*CAMBER(I)
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1005)  XFOIL(I),YUP,YXUP,YLO,YXLO,TH,CA
        WRITE(15,1005)  XFOIL(I),YUP,YXUP,YLO,YXLO,TH,CA

110 CONTINUE
    RETURN

1000 FORMAT(1x,'AIRFOIL GEOMETRY INFORMATION')
1001 FORMAT(1x,'PRINTOUT IN SIMILARITY VARIABLES',&
    &28X, 15HMAX THICKNESS =,F12.8)
1002 FORMAT(1x,'AIRFOIL VOLUME =',F12.8,32X,15HMAX CAMBER    =,F10.6//&
    &20X,14HUPPER  SURFACE,14X,14HLOWER  SURFACE)
1003 FORMAT(8X,1HX,1X,2(12X,1HF,9X,5HDF/DX),8X,9HTHICKNESS,4X,&
    &6HCAMBER)
1004 FORMAT(8X,1HX,1X,2(12X,1HY,9X,5HDY/DX),8X,9HTHICKNESS,4X,&
    &6HCAMBER)
1005 FORMAT(1X,F12.8,2X,2F12.8,2(3X,2F12.8))
1006 FORMAT(1x,'PRINTOUT IN PHYSICAL VARIABLES NORMALIZED BY CHORD ',&
    &'LENGTH', 3X, 15HMAX THICKNESS =,F10.6)

END

SUBROUTINE PRINT
!
!                        SUBROUTINE FOR MAIN OUTPUT PRINT CONTROL.
!                        PRINTS RELATIVE PARAMETERS AND CALLS
!                        SPECIALIZED PRINT/PLOT SUBROUTINES AS REQUIRED.
!
!                                 PRINTOUT DESCRIPTION
!                  IF PHYS IS .TRUE. THE FOLLOWING INFORMATION IS
!                  PRINTED IN PHYSICALLY SCALED VARIABLES (I.E. USUAL
!                  AERODYNAMIC TERMS)
!                       X,Y ARE COORDINATES NON-DIMENSIONALIZED BY
!                           AIRFOIL CHORD.
!                       CP = (P - P(INF))/Q(INF) = -2.*(DP/DX)
!                       CD = DRAG COEFF = D/Q(INF)*AIRFOIL CHORD
!                       CPSTAR = CRITICAL PRESSURE COEFFICIENT
!                  IF PHYS IS .FALSE. THE ABOVE INFORMATION IS PRINTED
!                  IN TRANSONIC SIMILARITY VARIABLES.
!
!                  PRINTOUT OF SONIC LINE ORDINATES IN SUBR. M1LINE
!                  INCLUDING PRINTPLOT OF SONIC LINE ORDINATES
!                  CALLED BY - TSFOIL.
! **********************************************************************
!
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    INTEGER        PSTART
    LOGICAL        PSAVE
    COMMON /COM11/ ALPHAO   , CLOLD   , DELTAO  , DUBO    , EMACHO  ,&
    &IMINO    , IMAXO   , IMAXI   , JMINO   , JMAXO   ,&
    &JMAXI    , PSAVE   , PSTART  ,TITLE(20),TITLEO(20),&
    &VOLO     , XOLD(100),YOLD(100)
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        FCR      , KUTTA
    COMMON /COM14/ CLSET    , FCR     , KUTTA   , WCIRC
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
    DIMENSION  TPH(6)  , SIM(8) , BCT(15) , FCP(14)
!
    DATA  TPH /4HSIMI,4HLARI,2HTY,4H PHY,4HSICA,2HL /
    DATA  SIM /4HCOLE,4H    ,4HSPRE,4HITER,4HKRUP,4HP   ,&
    &4HUSER,4H    /
    DATA  BCT /4HFREE,4H AIR,4H    ,4HSOLI,4HD WA,4HLL  ,4HFREE,&
    &4H JET,4H    ,4HSLOT,4HTED ,4HWALL,4HPORO,4HUS W,4HALL /
    DATA  FCP /4HFULL,4HY CO,4HNSER,4HVATI,4HVE. ,4H    ,4H    ,&
    &4HNOT ,4HCONS,4HERVA,4HTIVE,4H AT ,4HSHOC,4HK.  /
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,900)
    WRITE (15,900)
    IS = 1
    IF (PHYS) IS = 4
    IE = IS + 2
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,901) (TPH(I),I=IS,IE)
    WRITE (15,901) (TPH(I),I=IS,IE)
    WRITE (16,901) (TPH(I),I=IS,IE)
    IE = 2 * SIMDEF
    IS = IE - 1
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,902) (SIM(I),I=IS,IE)
    WRITE (15,902) (SIM(I),I=IS,IE)
    WRITE (16,902) (SIM(I),I=IS,IE)
    IE = 3 * BCTYPE
    IS = IE - 2
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,903) (BCT(I),I=IS,IE)
    WRITE (15,903) (BCT(I),I=IS,IE)
    WRITE (16,903) (BCT(I),I=IS,IE)
    IS = 8
    IF (FCR) IS = 1
    IE = IS + 6
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,904) (FCP(I),I=IS,IE)
    WRITE (15,904) (FCP(I),I=IS,IE)
    WRITE (16,904) (FCP(I),I=IS,IE)
    IF (KUTTA) GO TO 10
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,905)
    WRITE (15,905)
    WRITE (16,905)
    GO TO 20
10  CONTINUE
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,906)
    WRITE (15,906)
    WRITE (16,906)
20  CONTINUE
    ALPHVF = ALPHA * VFACT
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,911)
!      IF ( PHYS ) WRITE (6,909) EMACH, DELTA
!      WRITE (6,907) ALPHVF, AK
!      IF (AK .GT. 0.0) WRITE (6,908) DUB
!      IF ( PHYS ) WRITE(6,910)CPFACT,CDFACT,CMFACT,CLFACT,YFACT,VFACT

    WRITE (15,911)
    WRITE (16,911)
    IF ( PHYS ) WRITE (15,909) EMACH, DELTA
    IF ( PHYS ) WRITE (16,909) EMACH, DELTA
    WRITE (15,907) ALPHVF, AK
    WRITE (16,907) ALPHVF, AK
    IF (AK .GT. 0.0) WRITE (15,908) DUB
    IF (AK .GT. 0.0) WRITE (16,908) DUB
    IF ( PHYS ) WRITE(15,912)CPFACT,CDFACT,CMFACT,CLFACT,YFACT,VFACT
    IF ( PHYS ) WRITE(16,912)CPFACT,CDFACT,CMFACT,CLFACT,YFACT,VFACT
    CALL PRINT1
    CALL PRTMC
    CALL MACHMP
    IF ( ABORT1 ) GO TO 60
    CALL FIXPLT
    IF (BCTYPE .EQ. 1) GO TO 40
    IF (BCTYPE .EQ. 3) GO TO 40
    CALL PRTWAL
40  CONTINUE
    CALL M1LINE
    IF (PRTFLO .EQ. 1) GO TO 50
    CALL PRTFLD
50  CONTINUE
    CALL CDCOLE
60  CONTINUE
    RETURN
!
900 FORMAT(1H1)
901 FORMAT(14H0 PRINTOUT IN ,2A4,A2,11H VARIABLES.)
902 FORMAT(41H0 DEFINITION OF SIMILARITY PARAMETERS BY ,2A4)
903 FORMAT(25H0 BOUNDARY CONDITION FOR ,3A4)
904 FORMAT(27H0 DIFFERENCE EQUATIONS ARE ,7A4)
905 FORMAT(37H0 LIFT COEFFICIENT SPECIFIED BY USER.)
906 FORMAT(30H0 KUTTA CONDITION IS ENFORCED.)
907 FORMAT(13X,7HALPHA =,F12.7/17X,3HK =,F12.7)
908 FORMAT(2X,18HDOUBLET STRENGTH =,F12.7)
909 FORMAT(14X,6HMACH =,F12.7/13X,7HDELTA =,F12.7)
910 FORMAT(1H0,8X,38HPARAMETERS USED TO TRANSFORM VARIABLES/&
    &18X,20HTO TRANSONIC SCALING//12X,8HCPFACT =,F12.7/&
    &12X,8HCDFACT =,F12.7/12X,8HCMFACT =,F12.7/12X,8HCLFACT =,F12.7&
    &/13X,7HYFACT =,F12.7/ 13X,7HVFACT =,F12.7)
911 FORMAT(1H0)
912 FORMAT(8HCPFACT =,F12.7/&
    &12X,8HCDFACT =,F12.7/12X,8HCMFACT =,F12.7/12X,8HCLFACT =,F12.7&
    &/13X,7HYFACT =,F12.7/ 13X,7HVFACT =,F12.7)
END

SUBROUTINE PRINT1
!
!                        PRINTS PRESSURE COEFFICIENT AND MACH NUMBER
!                        ON Y=0 LINE, AND PLOTS CP ALONG SIDE OF PRINT.
!                        CALLED BY - TSFOIL, PRINT.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    COMMON /COM25/ CPL(100) , CPU(100) , IDLA
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    COMMON /COM30/ EM1L(100), EM1U(100), YM(100), REST(104)
!
    REAL LIFT
    DIMENSION  LINE1(60)  , TMAC(2)
!
    DATA  TMAC / 2HM1, 2HK1/ , IB /1H /, IL / 1HL/,IU /1HU/, IS/1H*/
    DATA  IBB/ 1HB/
!
    CL = LIFT (CLFACT)
    CM = PITCH (CMFACT)
    CPMIN =  1.0E37
    CPMAX = -CPMIN
    IEM = 0
    CJ01 = -Y(JLOW)/(Y(JUP)-Y(JLOW))
    CJ02 = Y(JUP)/(Y(JUP)-Y(JLOW))
!
    DO 10 I=IMIN,IMAX
        UL      = CJLOW*PX(I,JLOW) - CJLOW1*PX(I,JLOW-1)
        IF(I .GT. ITE)  UL=CJ01*PX(I,JUP) + CJ02*PX(I,JLOW)
        IF(I .LT. ILE)  UL=CJ01*PX(I,JUP) + CJ02*PX(I,JLOW)
        CPL(I)  = -2.0 * UL * CPFACT
        EM1L(I) = EMACH1(UL)
        IF (EM1L(I) .GT. 1.3) IEM = 1
        UU      = CJUP *PX(I,JUP)  - CJUP1 *PX(I,JUP+1)
        IF (I .GT. ITE) UU = UL
        IF (I .LT. ILE) UU = UL
        CPU(I)  = -2.0 * UU * CPFACT
        EM1U(I) = EMACH1(UU)
        IF (EM1U(I) .GT. 1.3) IEM = 1
        CPMAX   = AMAX1(CPMAX,CPU(I), CPL(I))
        CPMIN   = AMIN1(CPMIN,CPU(I), CPL(I))
10  CONTINUE
!
    CPLARG  = AMAX1(CPMAX,ABS(CPMIN))
    UNPCOL  = CPLARG / 29.
!
!                             LOCATE CP* FOR PRINTER PLOT
!
    COL          = -CPSTAR / UNPCOL
    NCOL         = SIGN((ABS(COL)+.5), COL)
    NCOLS        = NCOL + 30
!
!                             PRINT SINGLE VARIABLES
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,260)
!      IF (IREF .EQ. 2) WRITE (6,270)
!      IF (IREF .EQ. 1) WRITE (6,280)
!      IF (IREF .EQ. 0) WRITE (6,290)
!      WRITE (6,200) CL, CM, CPSTAR

    WRITE (15,260)
    IF (IREF .EQ. 2) WRITE (15,270)
    IF (IREF .EQ. 1) WRITE (15,280)
    IF (IREF .EQ. 0) WRITE (15,290)
    WRITE (15,200) CL, CM, CPSTAR
    WRITE (17,200) CL, CM, CPSTAR
    WRITE (16,201) CL, CM, CPSTAR
    IF (CPL(IMIN) .LT. CPSTAR .AND. CPL(IMIN+1) .GT. CPSTAR) GO TO 70
15  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,210)
    WRITE (15,210)
    KT = 2
    IF ( PHYS ) KT = 1
!
!                             PRINT COLUMN HEADERS.
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,220) TMAC(KT) , TMAC(KT)
    WRITE (15,220) TMAC(KT) , TMAC(KT)
!

    IPLOT = 0

    IF(IREF .EQ. 0) WRITE(17,340) EMACH,CL

    DO 40 I=IMIN,IMAX
!
        DO 30 K=1,60
            LINE1(K) = IB
30      CONTINUE
!
        COL          = -CPU(I) / UNPCOL
        NCOL         =  SIGN((ABS(COL) + .5) , COL)
        NCOLU        =  NCOL + 30
        LINE1(NCOLU) =  IU
!
        COL          = -CPL(I) / UNPCOL
        NCOL         =  SIGN((ABS(COL) + .5) , COL)
        NCOLL        =  NCOL + 30
        LINE1(NCOLL) =  IL
        IF (NCOLL .EQ. NCOLU) LINE1(NCOLL) = IBB
        IF (IABS(NCOLS) .LT. 61) LINE1(NCOLS) = IS
!
!     Changed to write to a file - Andy Ko 4/3/03
!      IF (I .EQ. ILE) WRITE (6,230)
!      WRITE (6,250) I, X(I), CPL(I), EM1L(I), CPU(I), EM1U(I), LINE1
!      IF (I .EQ. ITE) WRITE (6,240)

        IF (I .EQ. ILE) WRITE (15,230)
        WRITE (15,250) I, X(I), CPL(I), EM1L(I), CPU(I), EM1U(I), LINE1
        IF (I .EQ. ITE) WRITE (15,240)

!
!                             Save DATA FOR PLOTTING
!
        ! IF ((IREF.NE.0) .OR. (I.LT.ILE) .OR. (I.GT.ITE)) GO TO 40
        ! iplot = iplot + 1
        WRITE (17,330) iplot, X(I), CPU(i), EM1U(i),CPL(i),EM1L(i)


40  CONTINUE
!
    IF ( IEM .EQ. 0 ) GO TO 50
    IF ( PHYS ) WRITE (6,300)
50  CONTINUE
    DO 60 J=JMIN,JMAX
        YM(J) = Y(J) * YFACT
60  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,310) JMIN,JMAX,(YM(J),J=JMIN,JMAX)
    WRITE (15,310) JMIN,JMAX,(YM(J),J=JMIN,JMAX)

    WRITE (20,309) IMIN,IMAX,(X(J),J=IMIN,IMAX)
    WRITE (20,310) JMIN,JMAX,(YM(J),J=JMIN,JMAX)

    RETURN
70  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,320)
    WRITE (15,320)
    IF (IREF .EQ. 2) GO TO 15
    ABORT1 = .TRUE.
    RETURN
!
200 FORMAT(1H0,9X,4HCL =,F10.6/10X,4HCM =,F10.6/9X,5HCP* =,F10.6)
201 FORMAT(1H0,9X,4HCL =,F16.12/10X,4HCM =,F16.12/9X,5HCP* =,F16.12)
210 FORMAT(1H0,27X,5HLOWER,23X,5HUPPER/28X,4HY=0-,24X,4HY=0+)
220 FORMAT(3X,1HI,8X,1HX,10X,2HCP,10X,A2,14X,2HCP,10X,A2/)
230 FORMAT(25X,20HAIRFOIL LEADING EDGE,45X,20HAIRFOIL LEADING EDGE)
240 FORMAT(25X,21HAIRFOIL TRAILING EDGE,44X,21HAIRFOIL TRAILING EDGE)
250 FORMAT(1H ,I3,3F12.6,4X,2F12.6,2X,60A1)
260 FORMAT(60H1 FORCE COEFFICIENTS, PRESSURE COEFFICIENT, AND MACH NUM&
    &BER /2X,59H(OR SIMILARITY PARAMETER) ON BODY AND DIVIDING STREAM L&
    &INE.)
270 FORMAT(20X,11HCOARSE MESH)
280 FORMAT(20X,11HMEDIUM MESH)
290 FORMAT(20X,11H FINAL MESH)
300 FORMAT(20H0***** CAUTION *****/&
    &32H MAXIMUM MACH NUMBER EXCEEDS 1.3/&
    &69H SHOCK JUMPS IN ERROR IF UPSTREAM NORMAL MACH NUMBER GREATER T&
    &HAN 1.3)
309 FORMAT(1H0//9X,7HX(I) I=,I3,3H TO,I3/(6X,6F12.6))
310 FORMAT(1H0//9X,7HY(J) J=,I3,3H TO,I3/(6X,6F12.6))
320 FORMAT(1H0,//&
    &' DETACHED SHOCK WAVE UPSTREAM OF X-MESH,SOLUTION TERMINATED.')

330 format(2x,i3,2x,f7.4,2x,f10.5,2x,f7.4,2x,f10.5,2x,f7.4)
340 format(2x,'TSFOIL2',3x,'Mach = ',f7.3,3x,'CL = ',f7.3/&
    &4x,'i',5x,'X/C',8x,'Cp-up',5x,'M-up',6x,'Cp-low',4x,'M-low')
!
END

SUBROUTINE PRTFLD
!
!                        PRINTS PRESSURE COEFFICIENT, FLOW ANGLE AND
!                        MACH NUMBER IN FLOW FIELD. NUMBER OF J LINES
!                        PRINTED IS DETERMINED FROM THE INPUT VALUE OF
!                        PRTFLO.
!                             PRTFLO = 1 , NONE.
!                             PRTFLO = 2 , ALL J LINES EXCEPT J0.
!                             PRTFLO = 3 , THREE J LINES AROUND JERROR.
!                       CALLED BY - PRINT.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        OUTERR
    COMMON /COM18/ ERROR    , I1      , I2      , IERROR  , JERROR  ,&
    &OUTERR   , EMU(100,2)        , VC(100) ,&
    &WI       , DCIRC   , POLD(100,2)
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    COMMON /COM30/ JLIN(100), REST(304)
!
    DIMENSION  TMAC(2), CPPR(3), PYPR(3), YPRINT(3), PRT(10), EM1(3)
    DATA  TMAC / 2HM1, 2HK1/
    DATA  PRT  / 4HMACH,4H NUM,4HBERS,4H    ,4H    ,&
    &4HSIMI,4HLARI,4HTY P,4HARAM,4HETER/
    IF (PRTFLO.EQ. 2) GO TO 25
!                        LOCATE LINES AROUND JERROR.
    JL = 3
    IF (JERROR .EQ. JMIN) GO TO 10
    IF (JERROR .EQ. JLOW) GO TO 20
    IF (JERROR .EQ. JUP ) GO TO 10
    IF (JERROR .EQ. JMAX) GO TO 20
    JLIN(1) = JERROR - 1
    JLIN(2) = JERROR
    JLIN(3) = JERROR + 1
    GO TO 30
10  CONTINUE
    JLIN(1) = JERROR
    JLIN(2) = JERROR + 1
    JLIN(3) = JERROR + 2
    GO TO 30
20  CONTINUE
    JLIN(1) = JERROR - 2
    JLIN(2) = JERROR - 1
    JLIN(3) = JERROR
    GO TO 30
25  CONTINUE
    JL = JMAX - JMIN + 1
!                        FILL JLIN ARRAY WITH VALUES OF J.
    K = 1
    DO 28 J=JMIN,JMAX
        JLIN(K) = J
        K = K + 1
28  CONTINUE
30  CONTINUE
!
!                        PRINT FLOW FIELD IN 3 J LINES PER PAGE.
    DO 100 MPR = 1,JL,3
        MPREND = MIN0(MPR+2,JL)
        DO  40 M=MPR,MPREND
            MQ = M-MPR+1
            JQ = JLIN(M)
            YPRINT(MQ) = Y(JQ)*YFACT
40      CONTINUE
!                        WRITE PAGE HEADER.
        IS = 1
        IF ( PHYS ) IS = 6
        IE = IS + 4
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,900) (PRT(I),I=IS,IE) , CPSTAR
!      WRITE(6, 901) (JLIN(M),M=MPR,MPREND)
!      WRITE(6, 902)(YPRINT(M),M=1,MQ)

        WRITE (15,900) (PRT(I),I=IS,IE) , CPSTAR
        WRITE(15, 901) (JLIN(M),M=MPR,MPREND)
        WRITE(15, 902)(YPRINT(M),M=1,MQ)
        KT = 2
        IF ( PHYS ) KT = 1
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,903) TMAC(KT), TMAC(KT), TMAC(KT)
        WRITE (15,903) TMAC(KT), TMAC(KT), TMAC(KT)
        DO  60 I=IMIN,IMAX
            DO  50 M=MPR,MPREND
                MQ = M-MPR+1
                J = JLIN(M)
                U = PX(I,J)
                CPPR(MQ) = -2.0 * CPFACT * U
                PYPR(MQ) =  VFACT*PY(I,J)
                EM1(MQ)  = EMACH1(U)
50          CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,904)  I,X(I),(CPPR(M),PYPR(M),EM1(M),M=1,MQ)
            WRITE (15,904)  I,X(I),(CPPR(M),PYPR(M),EM1(M),M=1,MQ)
60      CONTINUE
100 CONTINUE
    RETURN
!
900 FORMAT(47H1PRESSURE COEFFICIENTS, FLOW ANGLES, AND LOCAL ,5A4/&
    &20H ON Y=CONSTANT LINES/9H CPSTAR =,F12.7//)
901 FORMAT(13X,3(15X,2HJ=,I4,15X))
902 FORMAT(13X,3(12X,2HY=,F10.6,12X))
903 FORMAT(4H0  I,8X,1HX,5X,3(6X,2HCP,8X,5HTHETA,7X,A2,6X)//)
904 FORMAT(1X,I3,2X,F10.6,1X,3(2X,3F11.6,1X))
!
END

SUBROUTINE PRTMC
!                        SUBROUTINE TO PRINT A CHARACTER FOR EACH
!                        POINT IN THE GRID DESCRIBING THE TYPE OF
!                        FLOW.   S , SHOCK POINT
!                                H , HYPERBOLIC POINT
!                                P , PARABOLIC POINT
!                                - , ELLIPTIC POINT
!                        CALLED BY - PRINT.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON /COM22/ CXC(100) , CXL(100), CXR(100), CXXC(100),CXXL(100),&
    &CXXR(100), C1(100)
!
    COMMON  /COM30/ IPC(100) , VT(100,2), REST(104)
    DATA  IHP/1HP/, IHH/1HH/, IHS/1HS/, IHD/1H-/, IB/1H /
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,100)
    WRITE (15,100)
    DO 5 I=1,50,2
        IPC(I)   = IB
        IPC(I+1) = IB
5   CONTINUE
    DO 10 J=JMIN,JMAX
        VT(J,1) = C1(2)
10  CONTINUE
    DO 60 K=JMIN,JMAX
        J = JMAX - K + 1
        DO 50 I=IUP,IDOWN
            VT(J,2) = VT(J,1)
            VT(J,1) = C1(I)-(CXL(I)*P(J,I-1)+CXC(I)*P(J,I)+CXR(I)*P(J,I+1))
            IF (VT(J,1) .GT. 0.0) GO TO 30
            IF (VT(J,2) .LT. 0.0) GO TO 20
!                        PARABOLIC POINT   (SONIC)
            IPC(I) = IHP
            GO TO 50
20          CONTINUE
!                        HYPERBOLIC POINT   (SUPERSONIC)
            IPC(I) = IHH
            GO TO 50
30          CONTINUE
            IF (VT(J,2) .LT. 0.0) GO TO 40
!                        ELLIPTIC POINT  (SUBSONIC)
            IPC(I) = IHD
            GO TO 50
40          CONTINUE
!                        SHOCK POINT
            IPC(I) = IHS
50      CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,110) J, (IPC(I),I=IUP,IDOWN)
        WRITE (15,110) J, (IPC(I),I=IUP,IDOWN)
60  CONTINUE
    RETURN
!
100 FORMAT(39H1 FLOW AT EACH GRID POINT.  P PARABOLIC/&
    &28X,12HH HYPERBOLIC/28X,7HS SHOCK/28X,10H- ELLIPTIC//)
110 FORMAT(10X,I3,5X,100A1)
!
END

SUBROUTINE PRTSK(Z,ARG,L,NSHOCK,CDSK,LPRT1)
!                  PRINTOUT WAVE DRAG CONTRIBUTION AND TOTAL PRESSURE
!                  LOSS ALONG SHOCK WAVE
!                  CALLED BY - CDCOLE.
!
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    DIMENSION Z(1), ARG(1)
    CDYCOF = -CDFACT*GAM1/(6.*YFACT)
    POYCOF = DELTA*DELTA*GAM1*(GAM1-1.)/12.
!     Changed to write to a file - Andy Ko 4/3/03
!      IF(NSHOCK .EQ.1) WRITE(6,1001)
!      WRITE(6,1002) NSHOCK, CDSK

    IF(NSHOCK .EQ.1) WRITE(15,1001)
    WRITE(15,1002) NSHOCK, CDSK
    DO 10 K = 1,L
        YY = Z(K)*YFACT
        CDY = CDYCOF*ARG(K)
        POY = 1. + POYCOF*ARG(K)
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1003) YY,CDY,POY
        WRITE(15,1003) YY,CDY,POY
10  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      IF(LPRT1 .EQ. 1) WRITE(6,1004)
    IF(LPRT1 .EQ. 1) WRITE(15,1004)
    RETURN
1001 FORMAT(74H1INVISCID WAKE PROFILES FOR INDIVIDUAL SHOCK WAVES WITHI&
    &N MOMENTUM CONTOUR)
1002 FORMAT(6H0SHOCK,I3/26H WAVE DRAG FOR THIS SHOCK=,F12.6/&
    &6X,1HY,9X,5HCD(Y),8X,8HPO/POINF/)
1003 FORMAT(1X,3F12.8)
1004 FORMAT(35H0SHOCK WAVE EXTENDS OUTSIDE CONTOUR/&
    &61H PRINTOUT OF SHOCK LOSSES ARE NOT AVAILABLE FOR REST OF SHOCK)
END

SUBROUTINE PRTWAL
!
!                        PRINTS PRESSURE COEFFICIENT AND FLOW ANGLE
!                        ON Y=-H AND Y=+H, AND PLOTS CP ALONG SIDE OF
!                        TABULATION.
!                        CALLED BY - PRINT.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM5/ XDIFF(100),YDIFF(100)
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM30/ CPLW(100), CPUW(100), VLW(100), VUW(100), REST(4)
!
    DIMENSION  BCT(15) , LINE1(60)
!
    DATA   IB /1H /, IL / 1HL/, IU /1HU/, IS / 1H*/, IBB /1HB/
    DATA  BCT /4H    ,4HFREE,4H AIR,4H  SO,4HLID ,4HWALL,4H    ,&
    &4HFREE,4H JET,4HSLOT,4HTED ,4HWALL,4H POR,4HOUS ,4HWALL/
!
!                             PRINT SINGLE VARIABLES
!
    I2 = 3 * BCTYPE
    I1 = I2 - 2
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,903) (BCT(I),I=I1,I2)
    WRITE (15,903) (BCT(I),I=I1,I2)
    THH = H * YFACT
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,904) THH
    WRITE (15,904) THH
    IF( BCTYPE .LT. 5 )  GO TO 4
    PORF = POR / YFACT
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,905) PORF
    WRITE (15,905) PORF
4   CONTINUE
    IF(  BCTYPE .NE. 4  .AND.  BCTYPE .NE. 6 )  GO TO 6
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,906) F
    WRITE (15,906) F
6   CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,907) CPSTAR
    WRITE (15,907) CPSTAR
!
    CPMIN =  1.0E37
    CPMAX = -CPMIN
!
    CPT = -2.0 * CPFACT
    DO 10 I=IUP,IDOWN
        CPLW(I) = CPT   * PX(I,JMIN)
        CPUW(I) = CPT   * PX(I,JMAX)
        CPMAX   = AMAX1(CPMAX,CPUW(I), CPLW(I))
        CPMIN   = AMIN1(CPMIN,CPUW(I), CPLW(I))
10  CONTINUE
    DO 20 I=IUP,IDOWN
        IF (BCTYPE .NE. 2) GO TO 11
!                                 SOLID WALL
        VLW(I) = 0.0
        VUW(I) = 0.0
        GO TO 20
11      CONTINUE
        IF (BCTYPE .NE. 3) GO TO 12
!                                 FREE JET
        VLW(I) = VFACT * PY(I,JMIN)
        VUW(I) = VFACT * PY(I,JMAX)
        GO TO 20
12      CONTINUE
        IF (BCTYPE .NE. 4) GO TO 13
!                                 SLOTTED WALL
        VLW(I) =  VFACT * FHINV * (P(JBOT,I) + .75 * CIRCFF)
        VUW(I) = -VFACT * FHINV * (P(JTOP,I) - .25 * CIRCFF)
        GO TO 20
13      CONTINUE
!                                 POROUS WALL
        IF (POR .GT. 1.5) GO TO 14
        VLW(I) =  VFACT * POR * XDIFF(I)*(P(JMIN,I)-P(JMIN,I-1))
        VUW(I) = -VFACT * POR * XDIFF(I)*(P(JMAX,I)-P(JMAX,I-1))
        GO TO 20
14      CONTINUE
        VLW(I) = VFACT *.25*(P(JMIN+1,I+1)+2.*P(JMIN+1,I)+P(JMIN+1,I-1)&
        &- P(JMIN  ,I+1)-2.*P(JMIN  ,I)-P(JMIN  ,I-1))&
        &/ (Y(JMIN+1)-Y(JMIN))
        VUW(I) = VFACT *.25*(P(JMAX,I+1)  +2.*P(JMAX,I)  +P(JMAX,I-1)&
        &- P(JMAX-1,I+1)-2.*P(JMAX-1,I)-P(JMAX-1,I-1))&
        &/ (Y(JMAX)-Y(JMAX-1))
20  CONTINUE
!
    CPLARG  = AMAX1(CPMAX,ABS(CPMIN))
    UNPCOL  = CPLARG / 29.
!
!                             LOCATE CP* FOR PRINTER PLOT
!
    COL          = -CPSTAR / UNPCOL
    NCOL         = SIGN((ABS(COL)+.5), COL)
    NCOLS        = NCOL + 30
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,210)
    WRITE (15,210)
!
!                             PRINT COLUMN HEADERS.
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,220)
    WRITE (15,220)
!
    DO 40 I=IUP,IDOWN
!
        DO 30 K=1,60
            LINE1(K) = IB
30      CONTINUE
!
        COL          = -CPUW(I) / UNPCOL
        NCOL         =  SIGN((ABS(COL) + .5) , COL)
        NCOLU        =  NCOL + 30
        LINE1(NCOLU) =  IU
!
        COL          = -CPLW(I) / UNPCOL
        NCOL         =  SIGN((ABS(COL) + .5) , COL)
        NCOLL        =  NCOL + 30
        LINE1(NCOLL) =  IL
        IF ( NCOLL .EQ. NCOLU ) LINE1(NCOLL) = IBB
        IF (IABS(NCOLS) .LT. 61) LINE1(NCOLS) = IS
!
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,250) I, X(I), CPLW(I), VLW(I), CPUW(I), VUW(I), LINE1
        WRITE (15,250) I, X(I), CPLW(I), VLW(I), CPUW(I), VUW(I), LINE1
40  CONTINUE
    RETURN
!
210 FORMAT(1H0,27X,5HLOWER,23X,5HUPPER/28X,4HY=-H,24X,4HY=+H)
220 FORMAT(3X,1HI,8X,1HX,10X,2HCP,9X,5HTHETA,12X,2HCP,9X,5HTHETA/)
250 FORMAT(1H ,I3,3F12.6,4X,2F12.6,2X,60A1)
903 FORMAT(2H1 ,3A4,20H BOUNDARY CONDITION.)
904 FORMAT(1H0,10X,24HH (TUNNEL HALF HEIGHT) =,F9.6)
905 FORMAT(1H0,11X,23HPOR (POROSITY FACTOR) =,F9.6)
906 FORMAT(1H0,14X,20HF (SLOT PARAMETER) =,F9.6)
907 FORMAT(1H0,29X,5HCP* =,F9.6)
!
END
FUNCTION PX(I,J)
!                  FUNCTION PX COMPUTES U = DP/DX AT POINT I,J
!                  CALLED BY - CDCOLE, DRAG, FINDSK, MACHMP, M1LINE,
!                              NEWSK,  PRINT1, PRTFLD, PRTWAL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM5/ XDIFF(100),YDIFF(100)
!
!                  TEST TO LOCATE END POINTS
    IF(I .EQ. IMIN) GO TO 10
    IF(I .EQ. IMAX) GO TO 20
!                  INTERIOR MESH POINT
    PJI = P(J,I)
    PX = .5*(XDIFF(I+1)*(P(J,I+1)-PJI)  + XDIFF(I)*(PJI-P(J,I-1)))
    RETURN
10  CONTINUE
!                  UPSTREAM BOUNDARY
    PX = 1.5*XDIFF(I+1)*(P(J,I+1)-P(J,I)) -&
    &0.5*XDIFF(I+2)*(P(J,I+2) - P(J,I+1))
    RETURN
20  CONTINUE
!                  DOWNSTREAM BOUNDARY
    PX = 1.5*XDIFF(I)*(P(J,I) - P(J,I-1))&
    &-0.5*XDIFF(I-1)*(P(J,I-1) - P(J,I-2))
    RETURN
END

FUNCTION PY(I,J)
!                  FUNCTION PY COMPUTES V = DP/DY AT POINT I,J
!                  CALLED BY - CDCOLE, PRTFLD, PRTWAL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON / COM5/ XDIFF(100),YDIFF(100)
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    COMMON /COM26/ PJUMP(100)
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
!                  TEST FOR END POINTS OR POINTS NEAR AIRFOIL SLIT
    IF(J .EQ. JMIN) GO TO 10
    IF(J .EQ. JLOW) GO TO 20
    IF(J .EQ. JUP ) GO TO 40
    IF(J .EQ. JMAX) GO TO 50
!                  I,J IS AN INTERIOR POINT
    PJI = P(J,I)
    PY = .5*(YDIFF(J+1)*(P(J+1,I)-PJI) + YDIFF(J)*(PJI-P(J-1,I)))
    RETURN
10  CONTINUE
!                  I,J IS ON LOWER BOUNDARY. USE ONE SIDED DERIVATIVE
    PY = 1.5* YDIFF(J+1)*(P(J+1,I) - P(J,I)) -&
    &0.5* YDIFF(J+2)*(P(J+2,I) - P(J+1,I))
    RETURN
20  CONTINUE
!                  I,J IS ON ROW OF MESH PIINTS BELOW AIRFOIL.
!
    VMINUS = YDIFF(J)*(P(J,I) - P(J-1,I))
!
!                  TEST TO SEE IF I,J IS AHEAD, UNDER, OR BEHIND SLIT.
    IF(I .LT. ILE) GO TO 21
    IF(I .GT. ITE) GO TO 22
!                  I,J IS UNDER AIRFOIL.  USE DERIVATIVE BOUNDARY
!                  CONDITION.
    IC = I - ILE + 1
    PY = .5 * (FXL(IC) - ALPHA + VMINUS)
    RETURN
21  CONTINUE
!                        I,J IS AHEAD OF AIRFOIL.
    PY = .5*((P(JUP,I) - P(JLOW,I)) * YDIFF(JUP) + VMINUS)
    RETURN
22  CONTINUE
!                    I,J IS BEHIND AIRFOIL
    PY = .5*((P(JUP,I) - PJUMP(I) - P(JLOW,I)) * YDIFF(JUP) + VMINUS)
    RETURN
40  CONTINUE
!                  I,J IS ON ROW OF MESH POINTS ABOVE AIRFOIL
    VPLUS = YDIFF(J+1)*(P(J+1,I) - P(J,I))
!                  TEST TO SEE IF I IS AHEAD OF, OVER, OR BEHIND
!                  AIRFOIL SLIT.
    IF(I .LT. ILE) GO TO 41
    IF(I .GT. ITE) GO TO 42
    IC = I - ILE + 1
    PY = .5 * (VPLUS + FXU(IC) - ALPHA)
    RETURN
41  CONTINUE
!                        I,J IS AHEAD OF AIRFOIL.
    PY = .5*((P(JUP,I) - P(JLOW,I)) * YDIFF(JUP) + VPLUS)
    RETURN
42  CONTINUE
!                  I,J IS BEHIND AIRFOIL
    PY = .5*((P(JUP,I) - PJUMP(I) - P(JLOW,I)) * YDIFF(JUP) + VPLUS)
    RETURN
50  CONTINUE
!                  I,J IS ON TOP ROW OF MESH POINTS. USE ONE SIDED
!                  FORMULA.
    PY = 1.5*YDIFF(J)*(P(J,I) - P(J-1,I))&
    &-  0.5*YDIFF(J-1)*(P(J-1,I) - P(J-2,I))
!
    RETURN
END


SUBROUTINE READIN
!
!                  CALLED BY - TSFOIL.
!
!                                      INPUT EXPLANATION
!***********************************************************************
!
!        Almost ALL INPUT IS READ IN THIS SUBROUTINE. THE ORDER IS AS DESCRIBED
!        BELOW (the airfoil coordinates for the Jameson input format are read
!        in BODY).
!        1.) ONE CARD OF TITLE INFORMATION. AN ?A? (ALPHANUMERIC) FORMAT
!            IS USED TO READ AND WRITE THIS INFORMATION. MULTIPLE CASES
!            MAY BE RUN WITH THIS PROGRAM AND THE DATA FOR EACH CASE
!            MUST START WITH THIS CARD. THE LAST CARD OF THE INPUT MUST
!            BE A CARD WITH THE WORD ?FINISHED? IN THE FIRST 8 COLUMNS.
!
!        2.) NAMELIST CONTAINING THESE PARAMETERS IS NOW READ. (SEE
!            FORTRAN MANUAL FOR DESCRIPTION OF NAMELIST INPUT). THE
!            BLOCK DATA SUBROUTINE SETS A DEFAULT VALUE, AS NOTED BELOW,
!            FOR ALL OF THESE PARAMETERS.
!            ONLY THE VALUES WHICH ARE DIFFERENT FROM THE PREVIOUS CASE
!            (OR DEFAULT) MUST BE INCLUDED, ALTHOUGH AT LEAST ONE VALUE
!            MUST BE INPUT BY NAMELIST FOR EACH CASE.
!                           * (F) = FLOATING POINT *
!                           * (I) = INTEGER        *
!                           * (L) = LOGICAL        *
!                           * (E) = EXPONENTIAL    *
!                                                                DEFAULT
!                                                                 VALUE
!                  AMESH  (L) OPTION FOR ANALYTICAL MESH CALC.      .F.
!                             .TRUE. X AND Y MESH VALUES ARE COMPUTED
!                                  WHEN AMESH = T, IMAXI AND JMAXI
!                                  SHOULD ALSO BE SUPPLIED. IMAXI AN
!                                  ODD NO. AND JMAXI AN EVEN NO.
!                                  (81 AND 40 HAVE BEEN USED).
!                             .FALSE. X AND Y POINTS ARE THE DEFAULT
!                                    VALUES OR THE VALUES SUPPLIED
!                                    BY THE USER THRU NAMELIST.
!                  EMACH  (F) FREESTREAM MACH NUMBER.               .75
!                             NOTE****EMACH MAY NOT BE = 1.0
!                  DELTA  (F) BODY THICKNESS RATIO.                 .115
!                  ALPHA  (F) ANGLE OF ATTACK (DEGREES IF PHYS=T)   .12
!                  AK     (F) TRANSONIC SIMILARITY PARAMETER.       0.0
!                             (INPUT REQUIRED ONLY IF PHYS = .F. )
!                  GAM    (F) RATIO OF SPECIFIC HEATS.              1.4
!                  SIMDEF (I) SIMILARITY DEFINITION.                  3
!                             =1 COLE
!                             =2 SPREITER
!                             =3 KRUPP
!                             =4 USER
!                  PRTFLO (I)  OPTION FOR PRINT OF FINAL FLOW FIELD.  1
!                             =1  NO FLOW FIELD PRINT.
!                             =2  ALL J LINES PRINTED.
!                             =3 PRINT 3 J LINES AROUND MAXIMUM ERROR
!                  PSTART (I) OPTION FOR INITIALIZING P ARRAY.       1
!                             =1 SET TO ZERO.
!                             =2 READ P FROM UNIT 7
!                             =3 USE P IN CORE (PREVIOUS CASE).
!                  PSAVE  (L) OPTION FOR SAVING RESTART BLOCK OF    .F.
!                               VALUES ON UNIT 3.
!                             =.TRUE. SAVE FOR RESTART.
!                             =.FALSE. DO NOT SAVE.
!                  FCR    (L) FULLY CONSERVATIVE RELAXATION OPTION  .T.
!                             =.TRUE. DIFFERENCE EQUATIONS ARE
!                                      FULLY CONSERVATIVE FORM.
!                             =.FALSE. DIFFERENCE EQUATIONS NOT
!                                      CONSERVATIVE AT SHOCK WAVES.
!                  KUTTA  (L) KUTTA CONDITION OPTION.               .T.
!                             =.TRUE. KUTTA CONDITION IS ENFORCED
!                             =.FALSE. LIFT COEFFICIENT SPECIFIED
!                                      BY USER.
!                  CLSET  (F) LIFT COEFFICIENT, USED IF KUTTA       .0
!                             IS FALSE.
!                  BCFOIL (I) OPTION FOR FOIL OR BODY GEOMETRY.      3
!                             =1 NACA00XX
!                             =2 PARABOLIC ARC.
!                             =3 ORDINATES (READ LATER IN NAMELIST
!                                IF DIFFERENT FROM DEFAULT VALUES WHICH
!                                ARE FOR THE KORN AIRFOIL).
!                             =4 JAMESON'S AIRFOIL INPUT FORMAT
!                             =5 diamond airfoil
!                  BCTYPE (I) DESCRIBES THE TYPE OF FLOW TO BE       1
!                             COMPUTED.
!                             =1 FREE AIR.
!                             =2 SOLID WALL.
!                             =3 FREE JET.
!                             =4 SLOTTED WALL.
!                             =5 POROUS WALL.
!                  F      (F) TUNNEL SLOT PARAMETER.                0.
!                  H      (F) TUNNEL HALF HEIGHT/CHORD RATIO.       0.
!                  POR    (F) WALL POROSITY FACTOR.                 .0
!                  PHYS   (L) TYPE OF SCALING TO USE FOR I/O.       .T.
!                             =.TRUE. PHYSICALLY SCALED VALUES.
!                             =.FALSE. TRANSONICALLY SCALED VALUES
!                             FOR PHYS = .F. , ALSO INPUT VALUE FOR AK.
!                  IMAXI  (I) NUMBER OF X-MESH POINTS(.LE. 100)     77
!                  JMAXI  (I) NUMBER OF Y-MESH POINTS(.LE. 100)     56
!                  IMIN   (I) X MESH POINT WHERE CALC IS TO START     1
!                  JMIN   (I) Y MESH POINT WHERE CALC IS TO START     1
!                  ICUT   (I) CONTROL FOR MESH CUT AND REFINEMENT.    2
!                             = 0  INPUT MESH IS USED TO CONVERGENCE.
!                             = 1  INPUT MESH MAY BE CUT ONCE.
!                             = 2  INPUT MESH MAY BE CUT TWICE.
!                  WE      (F)  3 VALUES FOR RELAXATION FACTOR FOR 1.8
!                               ELIPTIC PTS. 1-ST FOR COARSE MESH, 1.9
!                               2-ND FOR MED. MESH AND 3-RD FOR    1.95
!                               FINE MESH. DEFAULT VALUES ARE SUGGESTED
!                              VALUES, IN ORDER. IF SPECIFIED IN INPUT
!                              ALL THREE VALUES MUST BE GIVEN.
!                  WCIRC  (F) RELAXATION FACTOR FOR CIRCULATION.    1.0
!                  MAXIT   (I) MAXIMUM NUMBER OF ITERATION CYCLES  500
!                             ALLOWED.
!                  CVERGE (F) CONVERGENCE CRITERION FOR RESIDUALS .00001
!                             OF P.
!                  DVERGE (F) DIVERGENCE CRITERION FOR RESIDUALS    10.
!                             OF P.
!                  RIGF   (F) REIGLES RULE FOR BODY SLOPE.          0.0
!                  EPS     (F) COEFFICIENT OF PXT                   .2
!                  IPRTER (I) CONTROL FOR FREQUENCY OF PRINT OF      10
!                             LINE IN MESH WHERE ERROR IS LARGEST.
!                             I.E. IPRTER=10 , LINE WILL BE PRINTED
!                                  EVERY 10-TH ITERATION
!                  NWDGE  (I) CONTROL FOR VISCOUS WEDGE INCLUSION     0
!                             = 0  NO WEDGE
!                             = 1  MURMAN BUMP
!                             = 2  YOSHIHARA WEDGE
!                  REYNLD (E) REYNOLDS NUMBER BASED ON CHORD.    4.0E+6
!                             USED WHEN NWDGE = 1
!                  WCONST (F) WEDGE CONSTANT. USED WHEN NWDGE=1     4.0
!                  IFLAP  (I) CONTROL FOR FLAP DEFLECTION.            0
!                             FLAP INCLUDED WHEN IFLAP .NE. 0
!                  DELFLP (F) FLAP DEFLECTION ANGLE.                5.0
!                             POSITIVE DEGREES T.E. DOWN
!                  FLPLOC (F) LOCATION OF FLAP H.L., X/C           0.77
!
! NOTE***** WHEN ARRAYS ARE READ BY NAMELIST THE FULL ARRAY MUST BE
!            SET, I.E. IF ALL VALUES ARE NOT REQUIRED THE ARRAY MAY
!            BE FILLED USING MULTIPLE ZEROS. (N*0.0)
!                  XU     (F) ARRAY - X VALUES FOR UPPER BODY. USED
!                             IF BCFOIL = 3. KORN AIRFOIL USING ALL
!                             100 PTS.(UPPER) AND 75 (LOWER) IS DEFAULT.
!                  XL     (F) ARRAY - X VALUES FOR LOWER BODY.
!                  YU     (F) ARRAY - Y VALUES FOR UPPER BODY.
!                  YL     (F) ARRAY - Y VALUES FOR LOWER BODY.
!                  NU     (I) NUMBER OF POINTS TO USE FOR UPPER BODY 100
!                  NL     (I) NUMBER OF POINTS TO USE FOR LOWER BODY  75
!
!****** NOTE THIS PROGRAM USES A MESH REFINEMENT METHOD FOR DECREASING
!            COMPUTER TIME. FOLLOW THE RULES BELOW FOR CONSTRUCTING THE
!            X AND Y MESH TO TAKE FULL ADVANTAGE OF THIS FEATURE.
!                   IMAXI - ITE    SHOULD BE A MULTIPLE OF 4.
!                   ITE   - IMIN   SHOULD BE A MULTIPLE OF 4.
!                   JMAXI - JUP  + 1 SHOULD BE A MULTIPLE OF 4
!                   JLOW  - JMIN + 1 SHOULD BE A MULTIPLE OF 4
!                            (WHERE JLOW IS LAST POINT BELOW SLIT AND
!                                   JUP IS FIRST POINT ABOVE SLIT.)
!              WHERE ITE = I FOR X = 1.0 (OR POINT ON BODY CLOSEST TO
!                                         X = 1.0).
!            SUBROUTINE CKMESH INSPECTS THE X AND Y MESHES TO SEE IF
!            THIS IS TRUE AND, IF NOT, WILL MODIFY INPUT MESH IN SOME
!            CASES.
!
!                  XIN    (F) ARRAY - X MESH POINTS. LIMIT 100 PTS.
!                  YIN    (F) ARRAY - Y MESH POINTS. LIMIT 100 PTS.
!                             X AND Y MESH DEFAULT VALUES ARE KRUPP
!                                     BASIC GRID.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON / COM8/ CVERGE   , DVERGE  , IPRTER  , MAXIT   ,&
    &WE(3)    , EPS
    INTEGER        BCFOIL
    COMMON / COM9/ BCFOIL   , NL      , NU      , XL(100) , XU(100) ,&
    &YL(100)  , YU(100) , RIGF,IFLAP,DELFLP,FLPLOC
    COMMON /COM10/ YFREE(100),YTUN(100),GAM     , JMXF    , JMXT
    INTEGER        PSTART
    LOGICAL        PSAVE
    COMMON /COM11/ ALPHAO   , CLOLD   , DELTAO  , DUBO    , EMACHO  ,&
    &IMINO    , IMAXO   , IMAXI   , JMINO   , JMAXO   ,&
    &JMAXI    , PSAVE   , PSTART  ,TITLE(20),TITLEO(20),&
    &VOLO     , XOLD(100),YOLD(100)
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    LOGICAL        FCR      , KUTTA
    COMMON /COM14/ CLSET    , FCR     , KUTTA   , WCIRC
    COMMON /COM25/ CPL(100) , CPU(100) , IDLA
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM34/ NWDGE    , WSLP(100,2)       , XSHK(2,3)         ,&
    &THAMAX(2,3)        , AM1(2,3), ZETA(2,3)         ,&
    &NVWPRT(2), WCONST  , REYNLD  , NISHK
    common /com99/ iread
!
    DATA  DONE /4HFINI /
    DATA   IFIRST /0/
    NAMELIST /INP/ AK     , ALPHA  , AMESH  , BCFOIL , BCTYPE , CLSET,&
    &CVERGE , DELTA  , DVERGE , EMACH  , EPS    , F    ,&
    &FCR    , GAM    , H      , ICUT   , IMAXI  , IMIN ,&
    &IPRTER , JMAXI  , JMIN   , KUTTA  , MAXIT  , NL   ,&
    &NU     , PHYS   , POR    , PRTFLO , PSAVE  ,&
    &PSTART , RIGF   , SIMDEF , WCIRC  , WE     ,&
    &XIN    , YIN    , XL     , YL     , XU     , YU   ,&
    &NWDGE  , REYNLD , WCONST , IFLAP  , DELFLP ,&
    &FLPLOC , IDLA
!
    CHARACTER(20) IN_FILENAME
    CHARACTER(20) OUT_FILENAME
    CHARACTER(20) BASENAME

5   CONTINUE
    IF (IFIRST .NE. 0) GO TO 2

!     write(6,424)
!     read (5,428) FILENAME
!     FILENAME = 'tsfoil2.inp'
    CALL get_command_argument(1, IN_FILENAME)
    ! BASENAME = IN_FILENAME(1: scan(trim(IN_FILENAME),".", BACK= .true.))
    ! OUT_FILENAME = BASENAME//"out"
    OPEN(unit = 2, file = IN_FILENAME, status = 'old')
! !     Added extra print line to indicate program is running - Andy Ko 4/3/03
!     write(6,*) 'Running program now ... Please wait'
    iread = 2

424 FORMAT(/2x,'enter name of input data file')
428 FORMAT(A20)

    IFIRST = 1

2   CONTINUE
    TIME1 = TIME2
    ELPTM = TIME2 - TIME1
    IF (ELPTM .LT. .01) GO TO 3
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,930) ELPTM
    WRITE (15,930) ELPTM
3   CONTINUE
    READ (iread,900,END=999) TITLE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,901) TITLE
    WRITE(15,901) TITLE
    IF (TITLE(1) .EQ. DONE) GO TO 999
10  CONTINUE
    READ (iread,INP)
!     READ (iread,777) ALPHA,DELTA,EMACH,BCFOIL
! 777 FORMAT(3F10.5,I5)
    IF (PSTART .NE. 3) GO TO 13
!                        TEST TO SEE IF P ARRAY IN CORE IS USEABLE.
    IF ( .NOT. ABORT1) GO TO 13
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,905)
    WRITE (15,905)
    GO TO 5
13  CONTINUE
    IF ( PHYS ) AK = 0.0
    IF ( .NOT. AMESH ) GO TO 14
    CALL AYMESH
    GO TO 18
14  CONTINUE
    IF (YIN(JMIN) .NE. 0.0) GO TO 18
!                        IF YIN NOT READ IN NAMELIST, FILL YIN BY
!                        DEFAULT VALUE FOR TUNNEL OR FREE AIR CASE.
    IF (BCTYPE .NE. 1) GO TO 16
    JMAXI = JMXF
    DO 15 J=JMIN,JMAXI
        YIN(J) = YFREE(J)
15  CONTINUE
    GO TO 18
16  CONTINUE
    JMAXI = JMXT
    DO 17 J=JMIN,JMAXI
        YIN(J) = YTUN(J)
17  CONTINUE
18  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,906) EMACH, POR, IMIN, BCTYPE, AMESH
!      WRITE (6,907) DELTA, CLSET, IMAXI, BCFOIL, PHYS
!      WRITE (6,908) ALPHA, EPS  , JMIN , PSTART, PSAVE
!      WRITE (6,909) AK   , RIGF , JMAXI, PRTFLO, KUTTA
!      WRITE (6,910) GAM, WCIRC, MAXIT, IPRTER, FCR
!      WRITE (6,911) F, CVERGE, NU, SIMDEF
!      WRITE (6,912) H, DVERGE, NL, ICUT
!      WRITE (6,920) WE
!      IF (NWDGE .EQ. 1) WRITE(6,940) REYNLD,WCONST
!      IF (NWDGE .EQ. 2) WRITE(6,950)
!      IF (IFLAP .NE. 0) WRITE(6,960) DELFLP,FLPLOC
!      WRITE (6,913)
!      WRITE (6,919) (XIN(I),I=IMIN,IMAXI)
!      WRITE (6,914)
!      WRITE (6,919) (YIN(J),J=JMIN,JMAXI)
!      IF (BCFOIL .LE. 2) GO TO 19
!      IF (BCFOIL .eq. 5) GO TO 19
!      WRITE (6,915)
!      WRITE (6,919) (XU(I),I=1,NU)
!      WRITE (6,916)
!      WRITE (6,919) (YU(I),I=1,NU)
!      WRITE (6,917)
!      WRITE (6,919) (XL(I),I=1,NL)
!      WRITE (6,918)
!      WRITE (6,919) (YL(I),I=1,NL)

    WRITE (15,906) EMACH, POR, IMIN, BCTYPE, AMESH
    WRITE (15,907) DELTA, CLSET, IMAXI, BCFOIL, PHYS
    WRITE (15,908) ALPHA, EPS  , JMIN , PSTART, PSAVE
    WRITE (15,909) AK   , RIGF , JMAXI, PRTFLO, KUTTA
    WRITE (15,910) GAM, WCIRC, MAXIT, IPRTER, FCR
    WRITE (15,911) F, CVERGE, NU, SIMDEF
    WRITE (15,912) H, DVERGE, NL, ICUT
    WRITE (15,920) WE
    IF (NWDGE .EQ. 1) WRITE(15,940) REYNLD,WCONST
    IF (NWDGE .EQ. 2) WRITE(15,950)
    IF (IFLAP .NE. 0) WRITE(15,960) DELFLP,FLPLOC
    WRITE (15,913)
    WRITE (15,919) (XIN(I),I=IMIN,IMAXI)
    WRITE (15,914)
    WRITE (15,919) (YIN(J),J=JMIN,JMAXI)
    IF (BCFOIL .LE. 2) GO TO 19
    IF (BCFOIL .eq. 5) GO TO 19
    WRITE (15,915)
    WRITE (15,919) (XU(I),I=1,NU)
    WRITE (15,916)
    WRITE (15,919) (YU(I),I=1,NU)
    WRITE (15,917)
    WRITE (15,919) (XL(I),I=1,NL)
    WRITE (15,918)
    WRITE (15,919) (YL(I),I=1,NL)
19  CONTINUE
    GAM1 = GAM + 1.0
    IREF = 0
    IMAX = IMAXI
    JMAX = JMAXI
    IM1  = IMAX - 1
    JM1  = JMAX - 1
    IF (IMAXI .GT. 100 .OR.  JMAXI .GT. 100) CALL INPERR (1)
!                        ANY CALL TO INPERR CAUSES A MESSAGE TO BE
!                        PRINTED AND EXECUTION IS STOPPED.
!
!                        CHECK INPUT MESH FOR MONOTONICALLY INCREASING
!                        VALUES.
    DO 20 I=IMIN,IM1
        IF (XIN(I) .GE. XIN(I+1)) CALL INPERR (2)
20  CONTINUE
!
    DO 30 J=JMIN,JM1
        IF (YIN(J) .GE. YIN(J+1)) CALL INPERR (3)
30  CONTINUE
!
    IF (EMACH .LT.    .5 .OR. EMACH .GT.  2.0) CALL INPERR (4)
    IF (ALPHA .LT. -9.0  .OR. ALPHA .GT. 9.0 ) CALL INPERR (5)
    IF (DELTA .LT.   0.0 .OR. DELTA .GT.  1.0) CALL INPERR (6)
    IF (NWDGE .GT. 0 .AND. EMACH .GT. 1.0) CALL INPERR(8)
!
!
!                        COMPUTE ILE AND ITE (LEADING AND TRAILING EDGE)
!
    CALL ISLIT ( XIN )
!
!                        COMPUTE JLOW AND JUP (LOCATION OF BODY SLIT)
!
    CALL JSLIT ( YIN )
!
!
!                        CHECK NUMBER OF MESH POINTS, IF NOT ODD ADD
!                        POINTS TO APPROPRIATE AREAS TO MAKE ODD NO.
!
    CALL CKMESH
!
!                        CHECK BOUNDS OF YMESH FOR TUNNEL CALCULATIONS.
    IF (BCTYPE .EQ. 1) GO TO 90
    HTM = H - .00001
    HTP = H + .00001
    YS  = ABS(YIN(JMIN))
    YE  = ABS(YIN(JMAX))
    IF (YS .LT. HTM .OR. YS .GT. HTP) GO TO 40
    IF (YE .GE. HTM .AND. YE .LE. HTP) GO TO 90
40  CONTINUE
!                        RESCALE Y MESH TO -H,+H BOUNDS.
    TERM = -H / YIN(JMIN)
    DO 45 J=JMIN,JLOW
        YIN(J) = TERM * YIN(J)
45  CONTINUE
    TERM = H / YIN(JMAX)
    DO 50 J=JUP,JMAX
        YIN(J) = TERM * YIN(J)
50  CONTINUE
!
90  CONTINUE
!                        IF PSTART = 2 READ OLD VALUES FROM TAPE 7.
    IF (PSTART .NE. 2) GO TO 100
!
    REWIND 7
    READ (7,900) TITLEO
    READ  (7,902) IMAXO, JMAXO, IMINO, JMINO
    READ  (7,903) CLOLD, EMACHO, ALPHAO, DELTAO, VOLO, DUBO
    READ (7,903) (XOLD(I),I=IMINO,IMAXO)
    READ (7,903) (YOLD(J),J=JMINO,JMAXO)
    DO 60 I=IMINO,IMAXO
        READ (7,903) (P(J,I), J=JMINO,JMAXO)
60  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1000) TITLEO, IMINO,IMAXO,JMINO,JMAXO,CLOLD,EMACHO,
!     1              ALPHAO,DELTAO,VOLO, DUBO
    WRITE(15,1000) TITLEO, IMINO,IMAXO,JMINO,JMAXO,CLOLD,EMACHO,&
    &ALPHAO,DELTAO,VOLO, DUBO
100 CONTINUE
    RETURN
999 STOP
!
900 FORMAT(20A4)
901 FORMAT(1H1,4X,20A4)
902 FORMAT(4I5)
903 FORMAT(8F10.6)
904 FORMAT(1H0,5X,2A4)
905 FORMAT(21H0 CALCULATION ABORTED//&
    &43H OUTPUT OF PREVIOUS SOLUTION NOT AVAILABLE.)
906 FORMAT(1H0,4X,7HEMACH =,F9.5,5X,5HPOR =,F9.5,3X,6HIMIN =,I4,&
    &3X,8HBCTYPE =,I3,5X,8HAMESH = ,L1)
907 FORMAT(1H0,4X,7HDELTA =,F9.5,3X,7HCLSET =,F9.5,2X,7HIMAXI =,I4,&
    &3X,8HBCFOIL =,I3,6X,7HPHYS = ,L1)
908 FORMAT(1H0,4X,7HALPHA =,F9.5,5X,5HEPS =,F9.5,3X,6HJMIN =,I4,&
    &3X,8HPSTART =,I3,5X,8HPSAVE = ,L1)
909 FORMAT(1H0,7X,4HAK =,F9.5,4X,6HRIGF =,F9.5,2X,7HJMAXI =,I4,&
    &3X,8HPRTFLO =,I3,5X,8HKUTTA = ,L1)
910 FORMAT(1H0,6X,5HGAM =,F9.5,3X,7HWCIRC =,F9.5,2X,7HMAXIT =,I4,&
    &3X,8HIPRTER =,I3,7X,6HFCR = ,L1)
911 FORMAT(1H0,8X,3HF =,F9.5,2X,8HCVERGE =,F9.5,5X,4HNU =,I4,&
    &3X,8HSIMDEF =,I3)
912 FORMAT(1H0,8X,3HH =,F9.5,2X,8HDVERGE =,F9.1,5X,4HNL =,I4,&
    &5X,6HICUT =,I3)
913 FORMAT(1H0,4X,3HXIN)
914 FORMAT(1H0,4X,3HYIN)
915 FORMAT(1H0,15X,2HXU)
916 FORMAT(1H0,15X,2HYU)
917 FORMAT(1H0,15X,2HXL)
918 FORMAT(1H0,15X,2HYL)
919 FORMAT(4X,6F11.6)
920 FORMAT(1H0,7X,5HWE = ,F4.2,2(1H,,F4.2))
930 FORMAT(25H0  TIME TO RUN CASE WAS  ,F6.2,9H SECONDS.)
940 FORMAT(1H0,15X,12HMURMAN WEDGE,5X,8HREYNLD =,E10.3,5X,&
    &8HWCONST =,F9.5)
950 FORMAT(1H0,15X,15HYOSHIHARA WEDGE)
960 FORMAT(1H0,15X,17HFLAP IS DEFLECTED,F5.2,20H DEGREES FROM H.L. =,&
    &F6.3,8H TO T.E.)
1000 FORMAT(39H1P INITIALIZED FROM PREVIOUS RUN TITLED/&
    &1X,20A4/31H WHICH HAD THE FOLLOWING VALUES/&
    &8H IMIN  =,I4/8H IMAX  =,I4/8H JMIN  =,I4/8H JMAX  =,I4/&
    &8H CL    =,F12.8/8H EMACH =,F12.8/8H ALPHA =,F12.8/&
    &8H DELTA =,F12.8/8H VOL   =,F12.8/8H DUB   =,F12.8/)
!
END


SUBROUTINE RECIRC
!                  SUBROUTINE RECIRC COMPUTES THE FOLLOWING
!                  1.) JUMP IN P AT TRAILING EDGE = CIRCTE
!                  2.) CIRCULATION FOR FARFIELD BOUNDARY = CIRCFF
!                  3.) JUMP IN P ALONG SLIT Y=0, X .GT.1 BY LINEAR
!                      INTERPOLATION BETWEEN CIRCTE AND CIRCFF
!                  4.) P(J0,ITE) AND P(J0, ITE-1)
!                  CALLED BY - SOLVE.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON / COM7/ CJUP     , CJUP1   , CJLOW   , CJLOW1
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        FCR      , KUTTA
    COMMON /COM14/ CLSET    , FCR     , KUTTA   , WCIRC
    LOGICAL        OUTERR
    COMMON /COM18/ ERROR    , I1      , I2      , IERROR  , JERROR  ,&
    &OUTERR   , EMU(100,2)        , VC(100) ,&
    &WI       , DCIRC   , POLD(100,2)
    COMMON /COM22/ CXC(100) , CXL(100), CXR(100), CXXC(100),CXXL(100),&
    &CXXR(100), C1(100)
    COMMON /COM26/ PJUMP(100)
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
!                  COMPUTE JUMP IN POTENTIAL AT TRAILING EDGE
!
    CTEOLD=CIRCTE
    PUP = CJUP*P(JUP,ITE) - CJUP1*P(JUP+1,ITE)
    PLOW = CJLOW*P(JLOW,ITE) - CJLOW1*P(JLOW-1,ITE)
    CIRCTE = PUP - PLOW
!                  COMPUTE FAR FIELD CIRCULATION
    CIRCO = CIRCFF
    IF(KUTTA) CIRCFF = (1.-WCIRC)*CIRCO + CIRCTE*WCIRC
    IF(.NOT. KUTTA) CIRCFF = .5*CLSET/CLFACT
!                  FIX JUMP IN P AT AIRFOIL TRAILING EDGE IF KUTTA=.F.
!                  AND LIFT OF AIRFOIL EXCEEDS CLSET.  THIS TREATMENT
!                  ASSUMES  THAT CL IS INCREASING FROM BELOW CLSET OR
!                  IS NOT TOO MUCH ABOVE CLSET TO START CALCULATION.
!
    IF ( .NOT. KUTTA) CIRCTE = CIRCFF
    DCIRC=CIRCTE-CTEOLD
!
!                  SET JUMP IN P ALONG Y = 0, X .GT. 1
    FACTOR = (CIRCFF - CIRCTE)/(X(IMAX) - 1.)
    DO 35 I = ITE,IMAX
        PJUMP(I) = CIRCTE + (X(I) - 1.) * FACTOR
35  CONTINUE
!
    RETURN
END

SUBROUTINE REDUB
!                  SUBROUTINE REDUB COMPUTES DOUBLET STRENGTH
!                  FOR LIFTING FREE AIR FLOWS, DOUBLET STRENGTH IS SET
!                  EQUAL TO MODEL VOLUME.  FOR OTHER FLOWS, THE NON
!                  LINEAR CONTRIBUTION IS ADDED.
!                  CALLED BY - SOLVE.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON / COM5/ XDIFF(100),YDIFF(100)
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM30/ XI(100)  , ARG(100)  , REST(204)
!
    IF(BCTYPE .NE. 1) GO TO 10
    IF(BCTYPE .EQ. 1 .AND. ABS(CIRCFF) .LT. .0001) GO TO 10
    DUB = VOL
    RETURN
10  CONTINUE
!
!                  COMPUTE  DOUBLE INTEGRAL OF U*U OVER MESH DOMAIN FOR
!                  DOUBLET STRENGTH
!                  U = PX IS CENTERED MIDWAY BETWEEN X MESH POINTS.
!                  FIRST THE INTEGRAL (PX**2)DY IS CALCULATED FOR X
!                  HELD CONSTANT. THUS 1./(X(I+1)-X(I))**2 MAY BE
!                  PULLED OUT OF THE INTEGRAL WHICH IS CALCULATED BY
!                  THE TRAPEZOIDAL RULE. THE X INTEGRATION CORRESPONDS
!                  TO SUMMING THESE INTEGRALS, WHICH LIE MIDWAY BETWEEN
!                  X MESH POINTS, USING A MODIFIED TRAPEZODIAL RULE.
!
    IEND = IMAX - 1
    DBLSUM = 0.
    DO 50 I=IMIN,IEND
        NARG = 0
        DO 30 J = JMIN, JMAX
            NARG = NARG + 1
            TEMP = P(J,I+1) - P(J,I)
            ARG(NARG) = TEMP * TEMP
            XI(NARG) = Y(J)
30      CONTINUE
        CALL TRAP(XI,ARG,NARG,SUM)
        DBLSUM = DBLSUM + SUM * XDIFF(I+1)
50  CONTINUE
    DBLSUM = GAM1*.25*DBLSUM
    DUB = VOL + DBLSUM
    RETURN
END

SUBROUTINE REFINE
!                        ROUTINE TO EXPAND THE X-MESH AND Y-MESH TO
!                        DOUBLE THE NUMBER OF POINTS IN EACH. WILL HAVE
!                        TO BE CALLED TWICE IF MESH WAS HALVED TWICE.
!                        THE P(J,I) MESH IS ALSO FILLED BY
!                        INTERPOLATION.
!                        CALLED BY - TSFOIL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON /COM20/ XMID(100), YMID(100)
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM30/ PT(100)  , REST(304)
    COMMON /COM34/ NWDGE    , WSLP(100,2)       , XSHK(2,3)         ,&
    &THAMAX(2,3)        , AM1(2,3), ZETA(2,3)         ,&
    &NVWPRT(2), WCONST  , REYNLD  , NISHK
!
    XLEO = X(ILE)
    ILEO = ILE
    JMAXO = JMAX
    IMAX = 2 * (IMAX - IMIN) + IMIN
    JMAX = 2 * (JMAX - JMIN) + JMIN + 1
    IM2  = IMAX - 2
    JM2  = JMAX - 2
    IF (IREF .GT. 1) GO TO 30
    DO 10 I=IMIN,IMAX
        X(I) = XIN(I)
10  CONTINUE
    DO 20 J=JMIN,JMAX
        Y(J) = YIN(J)
20  CONTINUE
    IREF = 0
    GO TO 60
30  CONTINUE
    DO 40 I=IMIN,IMAX
        X(I) = XMID(I)
40  CONTINUE
    DO 50 J=JMIN,JMAX
        Y(J) = YMID(J)
50  CONTINUE
    IREF = 1
60  CONTINUE
    CALL ISLIT ( X )
    CALL JSLIT ( Y )
!                        SPREAD P(J,I) TO ALTERNATE I(X-MESH) POINTS.
    DO 90 J=JMIN,JMAXO
        K = IMIN - 1
        DO 70 I=IMIN,IMAX,2
            K = K + 1
            PT(I) = P(J,K)
70      CONTINUE
        DO 80 I=IMIN,IMAX,2
            P(J,I) = PT(I)
80      CONTINUE
90  CONTINUE
!                        SPREAD P(J,I) TO ALTERNATE J (Y-MESH) POINTS.
    DO 130 I=IMIN,IMAX,2
        K = JMIN - 1
        JE = JLOW - 1
        JL = JLOW - 2
        DO 95 J=JMIN,JE, 2
            K = K + 1
            PT(J) = P(K,I)
95      CONTINUE
        JST = JUP + 1
        DO 100 J=JST,JMAX,2
            K = K + 1
            PT(J) = P(K,I)
100     CONTINUE
        DO 110 J=JMIN,JE,2
            P(J,I) = PT(J)
110     CONTINUE
        DO 120 J=JST,JMAX,2
            P(J,I) = PT(J)
120     CONTINUE
130 CONTINUE
!                        INTERPOLATE TO FILL IN THE MISSING P VALUES.
    DO 140 I=IMIN,IM2
        PT(I) = (X(I+1)-X(I)) / (X(I+2)-X(I))
140 CONTINUE
    DO 150 J=JMIN,JE,2
        DO 145 I=IMIN,IM2,2
            P(J,I+1) = P(J,I) + PT(I) * (P(J,I+2) - P(J,I))
145     CONTINUE
150 CONTINUE
    DO 160 J=JST,JMAX,2
        DO 155 I=IMIN,IM2,2
            P(J,I+1) = P(J,I) + PT(I) * (P(J,I+2) - P(J,I))
155     CONTINUE
160 CONTINUE
    DO 170 J=JMIN,JM2
        PT(J) = (Y(J+1)-Y(J)) / (Y(J+2)-Y(J))
170 CONTINUE
    DO 190 I=IMIN,IMAX
        DO 175 J=JMIN,JL,2
            P(J+1,I) = P(J,I) + PT(J) * (P(J+2,I) - P(J,I))
175     CONTINUE
        DO 180 J=JST,JM2,2
            P(J+1,I) = P(J,I) + PT(J) * (P(J+2,I) - P(J,I))
180     CONTINUE
190 CONTINUE
!                  USE EXTRAPOLATION FOR JLOW,JUP
    D1 = Y(JLOW) - Y(JLOW-1)
    D2 = Y(JLOW-1) - Y(JLOW-2)
    CL1 = (D1 + D2) / D2
    CL2 = D1/D2
    D1 = Y(JUP+1) - Y(JUP)
    D2 = Y(JUP+2) - Y(JUP+1)
    CU1 = (D1 + D2) / D2
    CU2 = D1 / D2
    DO 200 I = IMIN,IMAX
        P(JUP,I) = CU1*P(JUP+1,I) - CU2*P(JUP+2,I)
        P(JLOW,I) = CL1*P(JLOW-1,I) - CL2*P(JLOW-2,I)
200 CONTINUE
!
!                        EXPAND VISCOUS WEDGE SLOPES TO NEW GRID
    IF (NWDGE .EQ. 0) RETURN
    INC = 0
    IF (X(ILE) .LT. XLEO) INC = 1
    M = 0
210 M = M + 1
    DO 220 I=IMIN,IMAX
220 PT(I) = 0.0
    ISTEP = ILEO - 1
    ISTRT = ILE + INC
    IEND = ITE + INC
    DO 230 I=ISTRT,IEND,2
        ISTEP = ISTEP + 1
        PT(I) = WSLP(ISTEP,M)
230 CONTINUE
    DO 240 I=ISTRT,IEND,2
        IM = I - 1
        IMM = IM - 1
        WSLP(I,M) = PT(I)
        RATIO = (X(IM)-X(IMM))/(X(I)-X(IMM))
        WSLP(IM,M) = PT(IMM)+(PT(I)-PT(IMM))*RATIO
240 CONTINUE
    IF (M .GE. 2) RETURN
    GO TO 210
END

SUBROUTINE RESET
!                  SUBROUTINE RESET  UPDATES FAR FIELD BOUNDARY
!                  CONDITIONS FOR SUBSONIC FREESTREAM FLOWS.
!                  CALLED BY - SOLVE.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    COMMON /COM24/ DTOP(100), DBOT(100),DUP(100), DDOWN(100),&
    &VTOP(100), VBOT(100),VUP(100), VDOWN(100)
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
!                  SET BOUNDARY CONDITIONS AT UPSTREAM AND DOWNSTREAM
!                  ENDS.
    K = JMIN - KSTEP
    DO 10 J = JMIN,JMAX
        K = K + KSTEP
        IF (J .EQ. JUP) K = K + KSTEP - 1
        P(J,IMIN) = CIRCFF*VUP(K) + DUB*DUP(K)
        P(J,IMAX) = CIRCFF*VDOWN(K) + DUB*DDOWN(K)
10  CONTINUE
    IF(BCTYPE .NE. 1) GO TO 25
!                  UPDATE BOUNDARY CONDITIONS ON TOP AND BOTTOM
    K = IMIN - KSTEP
    DO 20 I = IMIN,IMAX
        K = K + KSTEP
        P(JMIN,I) = CIRCFF*VBOT(K) + DUB*DBOT(K)
        P(JMAX,I) = CIRCFF*VTOP(K) + DUB*DTOP(K)
20  CONTINUE
25  RETURN
END

SUBROUTINE SAVEP
!
!                        SAVEP MOVES DATA INTO OLD DATA LOCATIONS AND
!                        WRITES IT ON TAPE3 IF REQUESTED.
!                        CALLED BY - TSFOIL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    INTEGER        PSTART
    LOGICAL        PSAVE
    COMMON /COM11/ ALPHAO   , CLOLD   , DELTAO  , DUBO    , EMACHO  ,&
    &IMINO    , IMAXO   , IMAXI   , JMINO   , JMAXO   ,&
    &JMAXI    , PSAVE   , PSTART  ,TITLE(20),TITLEO(20),&
    &VOLO     , XOLD(100),YOLD(100)
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
!                        RESET PARAMETERS SCALED IN SUBROUTINE SCALE.
    ALPHA = ALPHA * VFACT
    H = H * YFACT
    POR = POR / YFACT
    DO 6 J=JMIN,JMAX
        YIN(J) = YIN(J) * YFACT
6   CONTINUE
!                        MOVE RESTART DATA TO OLD BLOCK.
    DO 10 I=1,20
        TITLEO(I) = TITLE(I)
10  CONTINUE
    IMINO = IMIN
    JMINO = JMIN
    IMAXO  = IMAX
    JMAXO  = JMAX
    CLOLD  = CL
    EMACHO = EMACH
    ALPHAO = ALPHA
    DELTAO = DELTA
    VOLO   = VOL
    DUBO = DUB
!
    DO 20 I=IMINO,IMAXO
        XOLD(I) = X(I)
20  CONTINUE
!
    DO 30 J=JMINO,JMAXO
        YOLD(J) = YIN(J)
30  CONTINUE
!                        CHECK TO SEE IF RESTART IS TO BE WRITTEN ON
!                             TAPE3.
    IF ( .NOT. PSAVE) GO TO 100
40  CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03

    WRITE (15,900) TITLEO
    WRITE (15,901) IMAXO, JMAXO, IMINO, JMINO
    WRITE (15,902) CLOLD , EMACHO, ALPHAO, DELTAO, VOLO, DUBO
    WRITE (15,902) (XOLD(I),I=IMINO,IMAXO)
    WRITE (15,902) (YOLD(J),J=JMINO,JMAXO)
    DO 50 I=IMINO,IMAXO
        WRITE (15,902) (P(J,I),J=JMINO,JMAXO)

50  CONTINUE
100 CONTINUE
    RETURN
900 FORMAT(20A4)
901 FORMAT(4I5)
902 FORMAT(8F10.6)
!
END

SUBROUTINE SCALE
!
!                  SUBROUTINE SCALES PHYSICAL VARIABLES TO TRANSONIC
!                  VARIABLES.
!                  IF PHYS = .TRUE., ALL INPUT/OUTPUT QUANTITIES ARE IN
!                  PHYSICAL UNITS NORMALIZED BY FREESTREAM VALUES AND
!                  AIRFOIL CHORD. THIS SUBROUTINE THEN SCALES THE
!                  QUANTITIES TO TRANSONIC VARIABLES BY THE FOLLOWING
!                  CONVENTION
!                       SIMDEF = 1  COLE SCALING
!                       SIMDEF = 2  SPREITER SCALING
!                       SIMDEF = 3  KRUPP SCALING
!                       SIMDEF = 4  USER CHOICE
!                  IF PHYS = .FALSE., INPUT IS ALREADY IN SCALED
!                  VARIABLES AND NO FURTHER SCALING IS DONE.
!                  CALLED BY - TSFOIL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        AMESH
    COMMON / COM4/ XIN(100) , YIN(100), AMESH
    INTEGER        PSTART
    LOGICAL        PSAVE
    COMMON /COM11/ ALPHAO   , CLOLD   , DELTAO  , DUBO    , EMACHO  ,&
    &IMINO    , IMAXO   , IMAXI   , JMINO   , JMAXO   ,&
    &JMAXI    , PSAVE   , PSTART  ,TITLE(20),TITLEO(20),&
    &VOLO     , XOLD(100),YOLD(100)
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
!
    IF(PHYS) GO TO 50
!                  PHYS = .FALSE.  NO SCALING
    CPFACT = 1.
    CDFACT = 1.
    CLFACT = 1.
    CMFACT = 1.
    YFACT = 1.
    VFACT = 1.
    GO TO 600
!                  PHYS = .TRUE.  COMPUTE CONSTANTS
50  CONTINUE
    EMACH2 = EMACH*EMACH
    BETA  = 1. - EMACH2
    DELRT1 = DELTA**(1./3.)
    DELRT2 = DELTA**(2./3.)
!
!                  BRANCH TO APPROPRIATE SCALING
    GO TO (100,200,300,400), SIMDEF
!
!                  SIMDEF = 1
!                  COLE SCALING
100 CONTINUE
    AK = BETA/DELRT2
    YFACT = 1./DELRT1
    CPFACT = DELRT2
    CLFACT = DELRT2
    CDFACT = DELRT2*DELTA
    CMFACT = DELRT2
    VFACT = DELTA*57.295779
    GO TO 500
!                  SIMDEF = 2
!                  SPREITER SCALING
200 CONTINUE
    EMROOT = EMACH**(2./3.)
    AK = BETA/(DELRT2*EMROOT*EMROOT)
    YFACT = 1./(DELRT1*EMROOT)
    CPFACT = DELRT2/EMROOT
    CLFACT = CPFACT
    CMFACT = CPFACT
    CDFACT = CPFACT*DELTA
    VFACT = DELTA*57.295779
    GO TO 500
!                  SIMDEF = 3
!                  KRUPP SCALING
300 CONTINUE
    AK = BETA/(DELRT2*EMACH)
    YFACT = 1./(DELRT1*EMACH**.5)
    CPFACT = DELRT2/(EMACH**.75)
    CLFACT = CPFACT
    CMFACT = CPFACT
    CDFACT = CPFACT*DELTA
    VFACT = DELTA*57.295779
    GO TO 500
400 CONTINUE
!                  SIMDEF = 4
!                  THIS ADDRESS IS INACTIVE
!                  USER MAY INSERT SCALING OF OWN CHOICE
!                  DEFINITION FOR LOCAL MACH NUMBER MUST BE ADJUSTED
!                  IN EMACH1.
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1000)
    WRITE(15,1000)
1000 FORMAT(34H1ABNORMAL STOP IN SUBROUTINE SCALE/&
    &24H SIMDEF=4 IS NOT USEABLE)
    STOP
500 CONTINUE
!                  SCALE Y MESH
    YFACIV = 1.0 / YFACT
    DO 502 J=JMIN,JMAX
        YIN(J)  = YIN(J)  * YFACIV
502 CONTINUE
    IF (PSTART .EQ. 1) GO TO 505
    DO 504 J=JMINO,JMAXO
        YOLD(J) = YOLD(J) * YFACIV
504 CONTINUE
505 CONTINUE
!                  SCALE TUNNEL PARAMETERS
    H = H/YFACT
    POR = POR*YFACT
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,900) POR
    WRITE(15,900) POR
!
!                  SCALE ANGLE OF ATTACK
    ALPHA = ALPHA/VFACT
!
600 CONTINUE
!                  CHECK VALUE OF AK FOR DEFAULT.
    IF (AK .EQ. 0.0) CALL INPERR (7)
!                  COMPUTE SQUARE ROOT OF AK
    RTK = SQRT(ABS(AK))
!                  COMPUTE SONIC VELOCITY
    IF (ABS(GAM1).GT..0001) GO TO 999
    SONVEL=1.
    CPSTAR=0.
    RETURN
999 CONTINUE
    SONVEL = AK / GAM1
    CPSTAR = -2.0 * SONVEL * CPFACT
    RETURN
900 FORMAT(//10X,11HSCALED POR=,F10.5)
END

SUBROUTINE SETBC(IJUMP)
!                  SUBROUTINE SETBC SETS THE LIMITS ON RANGE OF I AND J
!                  FOR SOLUTION OF THE DIFFERENCE EQUATIONS.
!                  THE BODY SLOPE BOUNDARY CONDITION  AT THE CURRENT
!                  X MESH POINTS ON THE BODY ARE MULTIPLIED BY MESH
!                  SPACING CONSTANTS AND ENTERED INTO ARRAYS FXUBC AND
!                  FXLBC FOR USE IN SUBROUTINE SYOR.
!                  CALLED BY - TSFOIL.
!
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    COMMON / COM6/ FL(100)  , FXL(100), FU(100) , FXU(100),&
    &CAMBER(100), THICK(100),VOL    , XFOIL(100), IFOIL
    COMMON /COM17/ CYYBLC   , CYYBLD  , CYYBLU  , CYYBUC  , CYYBUD  ,&
    &CYYBUU   ,FXLBC(100),FXUBC(100), ITEMP1, ITEMP2
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM34/ NWDGE    , WSLP(100,2)       , XSHK(2,3)         ,&
    &THAMAX(2,3)        , AM1(2,3), ZETA(2,3)         ,&
    &NVWPRT(2), WCONST  , REYNLD  , NISHK
!
    IF (IJUMP .GT. 0) GO TO 20
!                  SET LIMITS ON I AND J INDICIES
    INT = 0
    IF(AK .LT. 0.) INT = 1
    IUP = IMIN + 1 + INT
    IDOWN = IMAX - 1 + INT
    JINT = 0
    IF(BCTYPE .EQ. 1 .AND. AK .GT. 0.) JINT = 1
    IF(BCTYPE .EQ. 3) JINT = 1
    IF(BCTYPE .EQ. 5 .AND. POR .GT. 1.5) JINT = 1
    JBOT = JMIN + JINT
    JTOP = JMAX - JINT
    J1   = JBOT + 1
    J2   = JTOP - 1
!                  AIRFOIL BODY BOUNDARY CONDITION
!                  ZERO ELEMENTS IN ARRAYS FOR UPPER AND LOWER BODY
!                  BOUNDARY CONDITIONS
20  DO 30 I = IMIN,IMAX
        FXLBC(I) = 0.
        FXUBC(I) = 0.
30  CONTINUE
!                  ENTER BODY SLOPES AT MESH POINTS ON AIRFOIL
!                  INTO ARRAYS FOR BODY BOUNDARY CONDITIONS
    IF(IREF .LE. 0) KSTEP = 1
    IF(IREF .EQ. 1)  KSTEP = 2
    IF(IREF .EQ. 2)  KSTEP = 4
    NFOIL = ITE - ILE + 1
    IF = IFOIL + KSTEP
    I = ITE + 1
    DO 50 N = 1,NFOIL
        I = I-1
        IF = IF - KSTEP
        FXLBC(I) = CYYBLU*(FXL(IF) - ALPHA + WSLP(I,2))
        FXUBC(I) = CYYBUD*(FXU(IF) - ALPHA + WSLP(I,1))
50  CONTINUE
    RETURN
END

SUBROUTINE SIMP(R,X,Y,N,IER)
!                  SUBROUTINE TO INTEGRATE BY SIMPSONS RULE.
!                  CALLED BY BODY.
    DIMENSION X(N),Y(N)
    R=0.0
    IF(N.GT.1) GO TO 1
    IER=2
    RETURN
1   IF(X(1).EQ.X(2)) GO TO 12
    NM1=N-1
    IF(N.EQ.2) GO TO 13
    IF(X(1).LT.X(2)) GO TO 3
!  TEST FOR X TO BE MONOTONICALLY DECREASING
    DO 2 I=2,NM1
        IF(X(I+1).GE.X(I)) GO TO 12
2   CONTINUE
    GO TO 5
!  TEST FOR X TO BE MONOTONICALLY INCREASING
3   DO 4 I=2,NM1
        IF(X(I+1).LE.X(I)) GO TO 12
4   CONTINUE
5   NM2=N-2
    IF(MOD(N,2).EQ.0) GO TO 14
    P=0.0
    N1=1
6   S1=X(N1+1)-X(N1)
    S2=X(N1+2)-X(N1+1)
    S3=X(NM1)-X(NM2)
    S4=X(N)-X(NM1)
    R=(2.*S1**2+S1*S2-S2**2)/S1*Y(N1)+(2.*S4**2+S3*S4-S3**2)/S4*Y(N)
    N1=N1+1
    DO 7 I=N1,NM1,2
        S1=X(I)-X(I-1)
        S2=X(I+1)-X(I)
7   R=R+(S1+S2)**3/(S1*S2)*Y(I)
    IF(N.LT.5) GO TO 9
    N1=N1+1
    DO 8 I=N1,NM2,2
        S1=X(I-1)-X(I-2)
        S2=X(I)-X(I-1)
        S3=X(I+1)-X(I)
        S4=X(I+2)-X(I+1)
8   R=R+((2.*S2**2+S1*S2-S1**2)/S2+(2.*S3**2+S3*S4-S4**2)/S3)*Y(I)
9   R=R/6.+P
10  CONTINUE
    IER=1
    RETURN
12  IER=4
    RETURN
!  TRAPEZOIDAL RULE FOR N=2
13  R=(X(2)-X(1))*(Y(1)+Y(2))/2.0
    GO TO 10
!  FIT POLYNOMIAL THRU FIRST 3 POINTS AND INTEGRATE FROM X(1) TO X(2).
14  S1=X(2)-X(1)
    S2=X(3)-X(1)
    S3=Y(2)-Y(1)
    S4=Y(3)-Y(1)
    P=S1/6.*(2.*S3+6.*Y(1)+(S2**2*S3-S1**2*S4)/(S2*(S2-S1)))
    N1=2
    GO TO 6
END

SUBROUTINE SOLVE
!
!                  SOLVE CONTROLS THE MAIN ITERATION LOOP.
!                  CALLED BY - TSFOIL.
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    LOGICAL        ABORT1
    COMMON / COM3/ IREF     , ABORT1   , ICUT    , KSTEP
    COMMON / COM8/ CVERGE   , DVERGE  , IPRTER  , MAXIT   ,&
    &WE(3)    , EPS
    COMMON /COM13/ CDFACT   , CLFACT  , CMFACT  , CPFACT  , CPSTAR
    LOGICAL        OUTERR
    COMMON /COM18/ ERROR    , I1      , I2      , IERROR  , JERROR  ,&
    &OUTERR   , EMU(100,2)        , VC(100) ,&
    &WI       , DCIRC   , POLD(100,2)
    COMMON /COM22/ CXC(100) , CXL(100), CXR(100), CXXC(100),CXXL(100),&
    &CXXR(100), C1(100)
    INTEGER        BCTYPE
    COMMON /COM28/ BCTYPE   , CIRCFF  , FHINV   , POR     , CIRCTE
    COMMON /COM32/ BIGRL    , IRL     , JRL
    COMMON /COM33/ THETA(100,100)
    COMMON /COM34/ NWDGE    , WSLP(100,2)       , XSHK(2,3)         ,&
    &THAMAX(2,3)        , AM1(2,3), ZETA(2,3)         ,&
    &NVWPRT(2), WCONST  , REYNLD  , NISHK
!
    REAL LIFT
    DATA   NDUB / 25 /
!
!
    ABORT1 = .FALSE.
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,600)
    WRITE(15,600)
!
    IF (IREF .EQ. 2) MAXITM = MAXIT / 4
    IF (IREF .EQ. 1) MAXITM = MAXIT / 2
    IF (IREF .EQ. 0) MAXITM = MAXIT
    KK = 3 - IREF
    WEP = WE(KK)
    WI = 1.0 / WEP
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,606) WEP, EPS, MAXITM
!
!      WRITE(6,601)

    WRITE (15,606) WEP, EPS, MAXITM
!
    WRITE(15,601)
    WRITE(19,601)
!
    DO 1 ITER = 1,MAXITM
!
!                  INITIALIZE EMU
        I1=1
        I2=2
        DO 5 J=JMIN,JMAX
            POLD(J,I2) = P(J,IUP-1)
            EMU(J,I2) = 0.0
5       CONTINUE
!
        IF ( AK .GT. 0.0 ) GO TO 7
        DO 6 J=JMIN,JMAX
            EMU(J,I2) = C1(2)
6       CONTINUE
7       CONTINUE
        OUTERR=.FALSE.
        IF(MOD(ITER,IPRTER) .EQ. 0)  OUTERR=.TRUE.
        ERROR=0.0
        BIGRL = 0.0
!                        UPDATE PJUMP.
        CALL RECIRC
!
        CALL SYOR
!
!                       UPDATE CIRCULATION FOR SUBSONIC FREESTREAM FLOW
!
        IF ( AK .LT. 0.0 ) GO TO 10
        IF (BCTYPE .NE. 1) GO TO 9
        IK = IUP - IMIN
        DO 8 I=IUP,IDOWN
            IK = IK + KSTEP
            JK = JBOT - JMIN
            DO 82 J=JBOT,JTOP
                JINC = KSTEP
                IF (Y(J) .LT. 0.0 .AND. Y(J+1) .GT. 0.0) JINC = 2 * KSTEP - 1
                JK = JK + JINC
                P(J,I) = P(J,I) + DCIRC * THETA(JK,IK)
82          CONTINUE
8       CONTINUE
9       CONTINUE
!******
        IF (MOD(ITER,NDUB) .EQ. 0) CALL REDUB
!******
        CALL RESET
10      CONTINUE
!                        COMPUTE VISCOUS WEDGE
        IF (NWDGE .GT. 0) CALL VWEDGE
        IF(OUTERR)  GO TO 2
        GO TO 1
2       CONTINUE
        CL = LIFT (CLFACT)
        CM = PITCH (CMFACT)
        ERCIRC=ABS(DCIRC)
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE (6,602) ITER, CL, CM, IERROR, JERROR, ERROR, IRL, JRL,
!     1             BIGRL, ERCIRC
        WRITE (15,602) ITER, CL, CM, IERROR, JERROR, ERROR, IRL, JRL,&
        &BIGRL, ERCIRC
        WRITE (19,602) ITER, CL, CM, IERROR, JERROR, ERROR, IRL, JRL,&
        &BIGRL, ERCIRC
!                        OUTPUT VISCOUS WEDGE QUANTITIES
        IF (NWDGE .EQ. 0) GO TO 20
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,612)
        WRITE(15,612)
        NN = NVWPRT(1)
        IF (NN .EQ. 0) GO TO 13
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,608)
        WRITE(15,608)
        DO 12 N=1,NN
            IF (AM1(1,N) .LE. 1.0) GO TO 11
            THA = THAMAX(1,N) *57.29578
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,610) N,XSHK(1,N),AM1(1,N),THA,ZETA(1,N)
            WRITE(15,610) N,XSHK(1,N),AM1(1,N),THA,ZETA(1,N)
            GO TO 12
!     Changed to write to a file - Andy Ko 4/3/03
!   11 WRITE(6,611) N
11          WRITE(15,611) N
12      CONTINUE
13      NN = NVWPRT(2)
        IF (NN .EQ. 0) GO TO 16
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,609)
        WRITE(15,609)
        DO 15 N=1,NN
            IF (AM1(2,N) .LE. 1.0) GO TO 14
            THA = THAMAX(2,N) *57.29578
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,610) N,XSHK(2,N),AM1(2,N),THA,ZETA(2,N)
            WRITE(15,610) N,XSHK(2,N),AM1(2,N),THA,ZETA(2,N)
            GO TO 15
!     Changed to write to a file - Andy Ko 4/3/03
!   14 WRITE(6,611) N
14          WRITE(15,611) N
15      CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!   16 IF (NISHK .EQ. 0) WRITE(6,607)
16      IF (NISHK .EQ. 0) WRITE(15,607)
        WRITE(6,601)
20      CONTINUE
        IF (ERROR .LE. CVERGE) GO TO 3
        IF (ERROR .GE. DVERGE) GO TO 4
1   CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,605)
    WRITE(15,605)
    WRITE(16,605)
    RETURN
3   CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,603)
    WRITE(15,603)
    WRITE(16,603)
    RETURN
4   CONTINUE
    ABORT1 = .TRUE.
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,604)
    WRITE(15,604)
    WRITE(16,604)
    RETURN
!
600 FORMAT(1H1)
601 FORMAT(/'  ITER',5X,2HCL,8X,2HCM,4X,4HIERR,1X,4HJERR,4X,&
    &5HERROR,4X,3HIRL,2X,3HJRL,4X,5HBIGRL,8X,6HERCIRC)
602 FORMAT(1X,I4,2F10.5,2I5,E13.4,2I4,2E13.4)
603 FORMAT(//20X,34H........SOLUTION CONVERGED........)
604 FORMAT(//20X,33H******  SOLUTION DIVERGED  ******)
605 FORMAT(//20X,39H******  ITERATION LIMIT REACHED  ******)
606 FORMAT(3X,5HWE = ,F7.4,5X,6HEPS = ,F8.4,5X,&
    &22HMAXIT FOR THIS MESH = ,I4)
607 FORMAT(5X,39HNO VISCOUS WEDGE, SINCE NO SHOCKS EXIST )
608 FORMAT(12H UPPER SHOCK,8X,3HX/C,10X,7HMACH NO,9X,5HTHETA,&
    &10X,4HZETA)
609 FORMAT(12H LOWER SHOCK,8X,3HX/C,10X,7HMACH NO,9X,5HTHETA,&
    &10X,4HZETA)
610 FORMAT(I9,4F15.5)
611 FORMAT(I9,5X,29HWEAK SHOCK, NO WEDGE INCLUDED)
612 FORMAT(10X,33HCOMPUTED VISCOUS WEDGE QUANTITIES)
!
END

SUBROUTINE SPLN1 (X,Y,N)
!
!                  CALLED BY - BODY.
    COMMON /SPLN/ A(200)    , B(200)  , DY1     , DY2     , K1      ,&
    &K2        , XP      , YP      ,DYP
!
!     CONTINUOUS DERIVATIVE INTERPOLATION SUBROUTINES
!     CURFIT COMPUTES COEFFICIENTS OF CUBICS  --  A(I)...I=1,2*N-2
!.....FOR THE WHOLE TABULATED TABLE
!     X(I) = INDEPENDENT VARIABLE.....I=1,N  (GIVEN)
!     Y(I) = DEPENDENT VARIABLE.....I=1,N  (GIVEN)
!     N = LENGTH OF Y-VS-X TABLE  (GIVEN)
!     DY1 = 1ST OR 2ND DERIVATIVE AT LOWER END OF TABLE
!     DY2 = 1ST OR 2ND DERIVATIVE AT UPPER END OF TABLE
!     K1 = 1 ......DY1 = 1ST DERIVATIVE  (GIVEN)
!     K1 = 2 ......DY1 = 2ND DERIVATIVE  (GIVEN)
!     K2 = 1 ......DY2 = 1ST DERIVATIVE  (GIVEN)
!     K2 = 2 ......DY2 = 2ND DERIVATIVE  (GIVEN)
!
    DIMENSION  X(100),  Y(100)
!
    N1=N-2
    C1=X(2)-X(1)
1   IF(K1.EQ.2)GO TO 4
2   B(1)=0.
    A(1)=(DY1-(Y(2)-Y(1))/C1)/C1
    GO TO 5
4   B(1)=-C1
    A(1)=-DY1/2.
5   J=1
    IF(N.EQ.2)GO TO 42
110 DO 10 I=1,N1
        J=J+1
        C1=X(I+1)-X(I)
        C2=X(I+2)-X(I+1)
        C3=Y(I+1)-Y(I)
        C4=Y(I+2)-Y(I+1)
        C5=C3/C1-C4/C2
        C6=C1/C2
        C7=C1*C2
        B(J)=1.0/(C6*(C1-B(J-1)))
        A(J)=(C5/C2-C6*A(J-1))*B(J)
        J=J+1
        B(J)=1.0/((-C1-C2)/C7-C6*B(J-1))
        A(J) =(-C5/C7-C6*A(J-1))*B(J)
10  CONTINUE
    IF(K2.EQ.2)GO TO 30
20  A(J+1)=(DY2-C4/C2+C2*A(J))/(C2*(B(J)-C2))
    GO TO 45
30  A(J+1)=(DY2/2.+A(J))/(-2.*C2+B(J))
    GO TO 45
!     STATEMENTS 42 TO 44 ARE FOR N=2 ONLY
42  C3=K1
    C2=1.0/C3
    IF(K2.EQ.2)GO TO 44
43  A(J+1)=((Y(2)-Y(1))/C1-A(J)*C1-DY2)/(C1*C1)*C2
    GO TO 45
44  A(J+1)=C3*((DY2+2.0*A(1))/(4.0*C1))
45  J=2*(N-1)
50  J=J-1
    IF(J.LE.0)RETURN
60  A(J)=A(J)-B(J)*A(J+1)
    GO TO 50
!
!
!   ENTRY POINT FOR INTERPOLATION
  ENTRY SPLN1X (X,Y,N)
!
    IF(XP.GT.X(1)) GO TO 11
!     SPECIAL CASE FOR EXTRAPOLATION BEYOND LOWER END OF X-TABLE
    C=X(2)-X(1)
    DYP=(Y(2)-Y(1))/C+A(1)*C
    YP=Y(1)+DYP*(XP-X(1))
    RETURN
11  IF(XP.LT.X(N)) GO TO 13
!     SPECIAL CASE FOR EXTRAPOLATION BEYOND UPPER END OF Y-TABLE
    C=X(N)-X(N-1)
    DYP=(Y(N)-Y(N-1))/C-A(2*N-3)*C-A(2*N-2)*C*C
    YP=Y(N)+DYP*(XP-X(N))
    RETURN
13  I=1
14  I=I+1
    IF(X(I).LT.XP) GO TO 14
!     NOW XP HAS BEEN BRACKETED SO THAT X(I-1).LT.XP.LE.X(I)
    C=XP-X(I-1)
    D=X(I)-XP
    K=2*I-3
    SLOPE=(Y(I)-Y(I-1))/(X(I)-X(I-1))
    YP=Y(I-1)+(SLOPE+(A(K)+A(K+1)*C)*D)*C
    DYP=SLOPE+A(K)*(D-C)+A(K+1)*(2.*D-C)*C
    RETURN
END

SUBROUTINE SYOR
!
!                  SYOR COMPUTES NEW P AT ALL MESH POINTS.
!                  CALLED BY - SOLVE.
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON / COM8/ CVERGE   , DVERGE  , IPRTER  , MAXIT   ,&
    &WE(3)    , EPS
    LOGICAL        FCR      , KUTTA
    COMMON /COM14/ CLSET    , FCR     , KUTTA   , WCIRC
    COMMON /COM17/ CYYBLC   , CYYBLD  , CYYBLU  , CYYBUC  , CYYBUD  ,&
    &CYYBUU   ,FXLBC(100),FXUBC(100), ITEMP1, ITEMP2
    LOGICAL        OUTERR
    COMMON /COM18/ ERROR    , I1      , I2      , IERROR  , JERROR  ,&
    &OUTERR   , EMU(100,2)        , VC(100) ,&
    &WI       , DCIRC   , POLD(100,2)
    COMMON /COM19/ DIAG(100), RHS(100), SUB(100), SUP(100)
    COMMON /COM22/ CXC(100) , CXL(100), CXR(100), CXXC(100),CXXL(100),&
    &CXXR(100), C1(100)
    COMMON /COM23/ CYYC(100), CYYD(100),CYYU(100), IVAL
    COMMON /COM26/ PJUMP(100)
    COMMON /COM32/ BIGRL    , IRL     , JRL
    DIMENSION     SAVE(100)
!
    IM2=IUP-1
    IF( AK .LT. 0.0 )  IM2=IUP-2
!
!
    J1 = JBOT + 1
    J2 = JTOP - JBOT
!
    DO 200 I=IUP,IDOWN
        EPSX = EPS/((X(I)-X(I-1))**2)
!
!                        COMPUTE VC = 1 - M**2
!
        DO 10 J=JBOT,JTOP
            VC(J) = C1(I) - (CXL(I)*POLD(J,I2) + CXC(I)*P(J,I)&
            &+  CXR(I)*P(J,I+1))
            EMU(J,I1) = 0.0
            POLD(J,I1) = P(J,I)
10      CONTINUE
        DO 20 J=JBOT,JTOP
            IF (VC(J) .LT. 0.0) EMU(J,I1) = VC(J)
20      CONTINUE
        IF ( FCR ) GO TO 22
        DO 21 J=JBOT,JTOP
            EMU(J,I2) = EMU(J,I1)
21      CONTINUE
22      CONTINUE
!
!                        COMPUTE ELEMENTS OF MATRIX
!
        DO 30 J=JBOT,JTOP
            DIAG(J) = (EMU(J,I1) - VC(J)) * CXXC(I) * WI&
            &+ EMU(J,I2) * CXXR(I-1) - CYYC(J)
            SUP(J)=CYYD(J)
            SUB(J)=CYYU(J)
30      CONTINUE
!
!                        COMPUTE RESIDUAL
!
        DO 40 J=JBOT,JTOP
            RHS(J)   = -(VC(J)-EMU(J,I1))*&
            &(CXXL(I)*P(J,I-1) - CXXC(I)*P(J,I) + CXXR(I)*P(J,I+1))
40      CONTINUE
        DO 50 J=JBOT,JTOP
            RHS(J)  = RHS(J) - (EMU(J,I2) * (CXXL(I-1)*P(J,IM2)&
            &- CXXC(I-1)*P(J,I-1) + CXXR(I-1)*P(J,I)))
50      CONTINUE
        JA = JBOT + 1
        JB = JTOP - 1
        DO 60 J=JA,JB
            RHS(J)   = RHS(J) - (CYYD(J)*P(J-1,I) - CYYC(J)*P(J,I)&
            &+ CYYU(J)*P(J+1,I))
60      CONTINUE
        RHS(JBOT) = RHS(JBOT) - (-CYYC(JBOT)*P(JBOT,I)&
        &+ CYYU(JBOT)*P(JBOT+1,I))
        IF (JBOT .EQ. JMIN) GO TO 61
        RHS(JBOT) = RHS(JBOT) - CYYD(JBOT)*P(JBOT-1,I)
61      CONTINUE
        RHS(JTOP) = RHS(JTOP) - (CYYD(JTOP)*P(JTOP-1,I)&
        &- CYYC(JTOP)*P(JTOP,I))
        IF (JTOP .EQ. JMAX) GO TO 62
        RHS(JTOP) = RHS(JTOP) - CYYU(JTOP)*P(JTOP+1,I)
62      CONTINUE
!
!                        CHECK FOR AIRFOIL B.C. AND KUTTA SLICE.
!
        IF (I .LT. ILE) GO TO 80
        IF (I .GT. ITE) GO TO 70
!
!                        AIRFOIL B. C.
!
        J=JUP
        DIAG(J)=DIAG(J)+CYYC(J)-CYYBUC
        SUP(J)=0.0
        SUB(J)=CYYBUU
        RHS(J)=RHS(J)+CYYD(J)*P(J-1,I)-CYYC(J)*P(J,I)+CYYU(J)*P(J+1,I)&
        &-(-CYYBUC*P(J,I)+CYYBUU*P(J+1,I)+FXUBC(I))
        J=JLOW
        DIAG(J)=DIAG(J)+CYYC(J)-CYYBLC
        SUP(J)=CYYBLD
        SUB(J)=0.0
        RHS(J)=RHS(J)+CYYD(J)*P(J-1,I)-CYYC(J)*P(J,I)+CYYU(J)*P(J+1,I)&
        &-(-CYYBLC*P(J,I)+CYYBLD*P(J-1,I)+FXLBC(I))
        GO TO 80
!
!                        KUTTA SLICE CHANGE.
70      CONTINUE
        RHS(JLOW) = RHS(JLOW) + CYYU(JLOW)*PJUMP(I)
        RHS(JUP)  = RHS(JUP)  - CYYD(JUP )*PJUMP(I)
80      CONTINUE
!
!                        INSERT WALL B. C.
!
        IVAL = I
        CALL BCEND
!
!                        COMPUTE MAX RESIDUAL.
!
        IF ( .NOT. OUTERR) GO TO 110
        DO 100  J=JBOT,JTOP
            ARHS = ABS(RHS(J))
            IF (ARHS .GT. BIGRL) GO TO 90
            GO TO 100
90          CONTINUE
            BIGRL = ARHS
            IRL   = I
            JRL   = J
100     CONTINUE
110     CONTINUE
!
!CCCCCCCC  ADD PXT
!
        DO 300 J = JBOT,JTOP
            DIAG(J) = DIAG(J) - EPSX
300     RHS(J) = RHS(J) - EPSX*(P(J,I-1)-POLD(J,I2))
!
!                        SOLVE TRIDIAGONAL MATRIX EQUATION.
!
        DNOM       = 1.0 / DIAG(JBOT)
        SAVE(JBOT)=SUB(JBOT)*DNOM
        RHS(JBOT)  = RHS(JBOT) * DNOM
        DO 120 J=J1,JTOP
            DNOM=1./(DIAG(J)-SUP(J)*SAVE(J-1))
            SAVE(J)=SUB(J)*DNOM
            RHS(J)=(RHS(J)-SUP(J)*RHS(J-1))*DNOM
120     CONTINUE
        DO 130 K=1,J2
            J      = JTOP - K
            RHS(J) = RHS(J) - SAVE(J) * RHS(J+1)
130     CONTINUE
!
!                        COMPUTE NEW P.
!
        DO 140 J=JBOT,JTOP
            P(J,I) = P(J,I) + RHS(J)
140     CONTINUE
!
!                        COMPUTE MAX ERROR
!
        IF ( .NOT. OUTERR) GO TO 180
        DO 170  J = JBOT,JTOP
            ARHS = ABS(RHS(J))
            IF (ARHS .GT. ERROR) GO TO 160
            GO TO 170
160         CONTINUE
            ERROR = ARHS
            IERROR = I
            JERROR = J
170     CONTINUE
180     CONTINUE
        IF (AK .GT. 0.0) GO TO 195
        IF (I .NE. IDOWN-1) GO TO 195
!
!                        SET P(IDOWN+1) = P(IDOWN-1) TO OBTAIN
!                        CENTERED VELOCITY AT IDOWN FOR SUPERSINIC
!                        FREESTREAM FLOW.
!
        DO 190 J=JMIN,JMAX
            P(J,IDOWN+1) = P(J,IDOWN-1)
190     CONTINUE
195     CONTINUE
        ISAVE = I2
        I2    = I1
        I1    = ISAVE
        IM2=I-1
200 CONTINUE
    RETURN
END

SUBROUTINE TRAP(X,Y,N,SUM)
!                  INTEGRATE Y DX BY TRAPEZODIAL RULE
!                  N IS THE NUMBER OF (X,Y) POINTS AND SUM IS THE
!                  RESULTING INTEGRAL.
!                  CALLED BY - CDCOLE, DRAG, PITCH, REDUB.
!                  INTEGRAL
    DIMENSION X(1),Y(1)
    SUM = 0.
    NM1 = N-1
    DO 10 I=1,NM1
        Z = X(I+1) - X(I)
        W = Y(I+1) + Y(I)
        SUM = SUM + Z*W
10  CONTINUE
    SUM = .5*SUM
    RETURN
END

SUBROUTINE VROOTS
!                  COMPUTE CONSTANTS BETA0,BETA1,BETA2,PSI0,PSI1,PSI2,
!                  USED IN FORMULA FOR VORTEX IN SLOTTED WIND TUNNEL
!                  WITH SUBSONIC FREESTREAM
!                  CALLED BY - FARFLD.
!
    COMMON /COM12/ F        , H       , HALFPI  , PI      , RTKPOR  ,&
    &TWOPI
    COMMON /COM15/ B        , BETA0   , BETA1   , BETA2   , PSI0    ,&
    &PSI1     , PSI2
!
    ERROR = .00001
!                  CALCULATE BETA0
    BETA0 = 0.
    DO 10 I = 1,100
        TEMP = BETA0
        Q = -F*TEMP + RTKPOR
        BETA0 = ATAN(Q)
        DBETA = ABS(TEMP - BETA0)
        IF(DBETA .LT. ERROR) GO TO 15
10  CONTINUE
    N = 0
    GO TO 9999
15  CONTINUE
!                  CALCULATE BETA1
    BETA1 = 0.
    DO 20 I=1,100
        TEMP = BETA1
        Q = -F*(TEMP + PI) + RTKPOR
        BETA1 = ATAN(Q)
        DBETA  = ABS(BETA1 - TEMP)
        IF(DBETA .LT. ERROR) GO TO 25
20  CONTINUE
    N = 1
    GO TO 9999
25  CONTINUE
!                  CALCULATE BETA2
    BETA2 = 0.
    DO 30 I=1,100
        TEMP = BETA2
        Q = -F*(TEMP - PI) + RTKPOR
        BETA2 = ATAN(Q)
        DBETA  = ABS(BETA2 - TEMP)
        IF(DBETA  .LT. ERROR) GO TO 35
30  CONTINUE
    N = 2
    GO TO 9999
35  CONTINUE
!                  COMPUTE PSI0,PSI1,PSI2
    TEMP = TAN(BETA0)
    PSI0 = (1. + F/(1. + TEMP*TEMP))
    PSI0 = 1.0 / PSI0
    TEMP = TAN(BETA1)
    PSI1 = (1. + F/(1.+ TEMP*TEMP))
    PSI1 = 1.0 / PSI1
    TEMP = TAN(BETA2)
    PSI2 = (1. + F/(1. + TEMP*TEMP))
    RETURN
!                  ABNORMAL STOP  IF ITERATIONS FOR BETAS DID NOT CONVER
9999 CONTINUE
!     Changed to write to a file - Andy Ko 4/3/03
!      WRITE(6,1000) N
    WRITE(15,1000) N
1000 FORMAT(35H1ABNORMAL STOP IN SUBROUTINE VROOTS/&
    &37H0NONCONVERGENCE OF ITERATION FOR BETA,  I1)
    STOP
END

SUBROUTINE VWEDGE
!
!                        COMPUTES MURMAN OR YOSHIHARA VISCOUS WEDGE AND
!                        MODIFYS SLOPE CONDITIONS TO ACCOUNT FOR JUMP
!                        IN DISPLACEMENT THICKNESS DUE TO SHOCK /
!                        BOUNDARY LAYER INTERACTION
    COMMON         P(102,101),X(100)  , Y(100)
    COMMON / COM1/ IMIN     , IMAX    , IUP     , IDOWN   , ILE     ,&
    &ITE      , JMIN    , JMAX    , JUP     , JLOW    ,&
    &JTOP     , JBOT    , J1      , J2
    COMMON / COM2/ AK       , ALPHA   , DUB     , GAM1    , RTK
    COMMON / COM5/ XDIFF(100),YDIFF(100)
    LOGICAL        PHYS
    INTEGER        PRTFLO   , SIMDEF
    COMMON /COM27/ CL       , DELTA   , DELRT2  , EMACH   , EMROOT  ,&
    &PHYS     , PRTFLO  , SIMDEF  , SONVEL  , VFACT   ,&
    &YFACT
    COMMON /COM34/ NWDGE    , WSLP(100,2)       , XSHK(2,3)         ,&
    &THAMAX(2,3)        , AM1(2,3), ZETA(2,3)         ,&
    &NVWPRT(2), WCONST  , REYNLD  , NISHK
!
!                        ZERO OUT PREVIOUS WEDGE SLOPES
    DO 5 J=1,2
        DO 3 I=ILE,ITE
3       WSLP(I,J) = 0.0
5   NVWPRT(J) = 0
    SIGN = 1.0
    LOC = JUP
    NISHK = 0
    N = 1
    ISTART = ILE
    JMP = 0
!                        LOCATE SHOCK ON UPPER SURFACE AND
!                             COMPUTE WEDGE IF SHOCK EXISTS
    M = 1
10  CALL FINDSK(ISTART,ITE,LOC,ISK)
    IF (ISK .LT. 0) GO TO 50
    NISHK = NISHK + 1
    NVWPRT(M) = NVWPRT(M) + 1
!                        COMPUTE X POSITION OF SHOCK BY INTERPOLATION
    V1 = PX(ISK-1,LOC)
    XSHK(M,N) = X(ISK-1)+(SONVEL-V1)/((PX(ISK,LOC)-V1)*XDIFF(ISK))
!                        COMPUTE FLOW PROPERTIES 3 POINTS UPSTREAM
    ISK3 = ISK - 3
    U = PX(ISK3,LOC)
    AM1(M,N) = EMACH1(U)
    AM1SQ = AM1(M,N) *AM1(M,N)
    IF (AM1SQ .LE. 1.0) GO TO 40
    THAMAX(M,N) = WANGLE(AM1SQ,NWDGE,GAM1)*SIGN
!                        IF NWDGE = 2 ,COMPUTE YOSHIHARA WEDGE
    IF (NWDGE .EQ. 1) GO TO 14
    ISK1 = ISK-1
    DO 12 I=ISK1,ISK
        WSLP(I,M) = THAMAX(M,N)/DELTA
12  CONTINUE
    GO TO 35
14  REYX = REYNLD*XSHK(M,N)
    CF = 0.02666/(REYX**0.139)
    DSTAR1 = 0.01738*REYX**0.861/REYNLD
    IF ((N.LE.1) .OR. (JMP.EQ.1)) GO TO 20
    DXS = XSHK(M,N) -XSHK(M,N-1)
    IF (DXS .GE. ZETA(M,N-1)) GO TO 15
    AETA = DXS/ZETA(M,N-1)
    DSTAR1 = DXS*THAMAX(M,N-1)*(1.0+AETA*(AETA/3.0 -1.0))
    GO TO 20
15  DSTAR1 = ZETA(M,N-1)*THAMAX(M,N-1)/3.0
20  CONTINUE
    JMP = 0
    ZETA(M,N) = WCONST*SQRT((AM1SQ-1.0)/CF) *DSTAR1
!                        COMPUTE WEDGE SLOPES
    XEND = XSHK(M,N) +ZETA(M,N)
    DO 30 I=ISK,ITE
        IF (X(I) .GE. XEND) GO TO 35
        AETA = (X(I) -XSHK(M,N))/ZETA(M,N)
        WSLP(I,M) = THAMAX(M,N) *(1.0-AETA)**2 /DELTA
30  CONTINUE
!                        CHECK FOR ADDITIONAL SHOCK ON SURFACE
35  N = N + 1
    IF (N .GE. 4) GO TO 50
    ISTART = ISK + 2
    GO TO 10
40  JMP = 1
    GO TO 35
50  IF (M .EQ. 2) GO TO 60
    N = 1
    ISTART = ILE
    LOC = JLOW
    SIGN = -SIGN
    M = 2
!                        RETURN TO COMPUTE LOWER SURFACE WEDGE
    GO TO 10
60  CONTINUE
    CALL SETBC(1)
    RETURN
END
FUNCTION WANGLE(AM2,NW,G)
    IF (NW .EQ. 2) GO TO 10
    WANGLE = 4.0*((AM2-1.0)/3.0)**1.5/G
    RETURN
10  AM3 = 3. * AM2
    AM4 = 4. * AM2
    AM7 = 7. * AM2
    RM = SQRT(3.*(AM3*AM2+AM4+20.))
    RS = SQRT(3.*(AM3*AM2-AM4+13.))
    S2TM = (AM3-5.+RM)/AM7
    S2TS = (AM3-2.+RS)/AM7
    TM = ASIN(SQRT(S2TM))
    TS = ASIN(SQRT(S2TS))
    TTM = TAN(TM)
    TTS = TAN(TS)
    TDM = 5.*(AM2*S2TM-1.)/(TTM*(5.+AM2*(6.-5.*S2TM)))
    TDS = 5.*(AM2*S2TS-1.)/(TTS*(5.+AM2*(6.-5.*S2TS)))
    WANGLE = 0.5*(ATAN(TDM)+ATAN(TDS))
    RETURN
END

SUBROUTINE DLAOUT(ILE,ITE,ALPHA,DFLP,EM,VF,RE)
!     OUTPUTS CP DATA IN FORM USABLE BY ANTANI'S INTEGRATION PROGRAM
    COMMON        P(102,101),X(100),Y(100)
    COMMON /COM25/ CPL(100),CPU(100),IDLA
    COMMON /COM30/ XCP(100),CPP(304)
    COMMON /SPLN/ A(200),B(200),DY1,DY2,K1,K2,XP,YP,DYP
    READ(5,100) NUP,NDOWN,NCON,NTEST,NRUN,NPT
    R=RE/1.E+06
    ALFA=ALPHA*VF
    WRITE(10,101) EM,ALFA,R,NCON,DFLP,NTEST,NRUN,NPT
    READ(5,102) (XCP(I),I=1,NUP)
    NS=NUP+1
    NE=NUP+NDOWN
    READ(5,102) (XCP(I),I=NS,NE)
    K1=1
    K2=1
    DY1=(CPU(ILE+1)-CPU(ILE))/(X(ILE+1)-X(ILE))
    DY2=(CPU(ITE)-CPU(ITE-1))/(X(ITE)-X(ITE-1))
    NX=ITE-ILE+1
    CALL SPLN1(X(ILE),CPU(ILE),NX)
    DO 10 I=1,NUP
        XP=XCP(I)
        CALL SPLN1X(X(ILE),CPU(ILE),NX)
        CPP(I)=YP
10  CONTINUE
    DY1=(CPL(ILE+1)-CPL(ILE))/(X(ILE+1)-X(ILE))
    DY2=(CPL(ITE)-CPL(ITE-1))/(X(ITE)-X(ITE-1))
    CALL SPLN1(X(ILE),CPL(ILE),NX)
    DO 20 I=NS,NE
        XP=XCP(I)
        CALL SPLN1X(X(ILE),CPL(ILE),NX)
        CPP(I)=YP
20  CONTINUE
    WRITE(10,102) (CPP(I),I=1,NUP)
    WRITE(10,102) (CPP(I),I=NS,NE)
    RETURN
100 FORMAT(6I5)
101 FORMAT(F6.3,F6.2,24X,F6.3,I6,F6.1,16X,2I3,I2)
102 FORMAT(11F6.3)
END