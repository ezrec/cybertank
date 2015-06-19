* CyberTank: an old Fortran game I wrote back in high school.
* 
* Jason "The Comptroller" McMullan <jason.mcmullan@gmail.com>
*
* Full screen, uses SMG$ utilities, even has a pop-up if you get
* VMS MAIL or PHONE messages!
*
* I suggest playing at level 7 with a friend - and watch out for
* the nuclear meltdown endgame!
*
* Copyright 1991-2007, Jason McMullan and David Rudy
*
**** FORTRAN SOURCE ****
*
* Jason McMullan and David Rudy's ultra cool CyberTank game
* (AKA the Harkrider ForTran Final)
* Copyright 1991, Jason McMullan and David Rudy
*
*  This is "keep our names in it" ware.  Do anything you like to
* this code, use it for whatever you want, just so long as you 
* credit us for original authorship.
*     Include '(SMG$ROUTINES)'
*     Include '(OTS$ROUTINES)'
*     Include '(STR$ROUTINES)'

      Common  /VARS/iGun,iPits,iMines,iHills,ivTank,lOmni
      Logical lOmni

      Common  /VIDEO/iPaste,iKeybd,iPlay

      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)

      Common  /ARRAYS/iaTank,iaR,iaC,iaCoolant,iaHp,iaStat
      Integer iaTank(2)
      Integer iaR(2,10)
      Integer iaC(2,10)
      Integer iaCoolant(2,10)
      Integer iaHp(2,10)
      Integer iaStat(2)

      Logical lDone/.FALSE./

*     Integer OutAst*4
*     External OutAST
*     Integer BroadAST*4
*     External BroadAST

      Call SMG$CREATE_PASTEBOARD(iPaste)
      Call SMG$CREATE_VIRTUAL_KEYBOARD(iKeybd)
      Call SMG$SET_KEYPAD_MODE(iKeybd,0)
      I=(2**25)-(2**18)-(2**13)-(2**9)-1
*     Call SMG$SET_OUT_OF_BAND_ASTS(iPaste,I,OutAST)
*     Call SMG$SET_BROADCAST_TRAPPING(iPaste,BroadAST)
 
      iSeed=SECNDS(0.0)
      lDone=.FALSE.
      Do While (lDone.NEQV..True.)
        Call GetInfo(iLevel)
        iWinner=iDoLevel(iLevel)
        If (iWinner.NE.0) Then
          Call Defeat(iWinner,lDone)
        End If
      End Do

      Call SMG$DELETE_PASTEBOARD(iPaste)
      Call SMG$DELETE_VIRTUAL_KEYBOARD(iKeybd)
      Stop
      End

* Function that releases proc time while waiting for a character..
      Function iGetKey()
      Common  /VIDEO/iPaste,iKeybd,iPlay
      Call SMG$READ_KEYSTROKE(iKeybd,iKey,,100)
      iGetKey=iKey
      Return
      End

*Subroutine That does the End-of game sequence
      Subroutine Defeat(iWinner,lDone)
      Logical lDone
      Character Ln(8)*40
      Common  /VIDEO/iPaste,iKeybd,iPlay

      Call SMG$CREATE_VIRTUAL_DISPLAY(13,40,iDefeat,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iDefeat,'Winner!')
      Ln(1)=' '
      Ln(2)='    ____________________'
      Ln(3)='    __________________   \\_____'
      Ln(4)='        _____/_________________\\______'
      Ln(5)='   ____/_       Player '//CHAR(iWinner+48)
     -                            //'     ____     |'
      Ln(6)='  / /    \\__________________/    \\    |'
      Ln(7)=' | / (0)______________________(0) \\   /'
      Ln(8)='  \\_\\______________________________/_/'
      Do I=1,8
        Call SMG$PUT_LINE(iDefeat,Ln(I))
      End Do
      Call SMG$PUT_LINE(iDefeat,Ln(1))
      Call SMG$PUT_LINE(iDefeat,Ln(1))
      Call SMG$PASTE_VIRTUAL_DISPLAY(iDefeat,iPaste,5,19)
      iKey=0
      Do While ((iKey.NE.89).AND.(iKey.NE.78))
        Call SMG$PUT_CHARS(iDefeat,'Play again [Y,N]: ',
     -                     13,1,1,SMG$M_FORWARD)
        iKey=iGetKey()
        if (iKey.GT.92) iKey=iKey-32
      End Do
      lDone=.NOT.(iKey.EQ.89)
      Call SMG$POP_VIRTUAL_DISPLAY(iDefeat,iPaste)
      Return
      End      

* Subroutine that does everything and more
      Function iDoLevel(iLevel)
      Include '($SMGDEF)'
      Common  /VARS/iGun,iPits,iMines,iHills,ivTank,lOmni
      Logical lOmni

      Common  /VIDEO/iPaste,iKeybd,iPlay
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)

      Common  /ARRAYS/iaTank,iaR,iaC,iaCoolant,iaHp,iaStat
      Integer iaTank(2)
      Integer iaR(2,10)
      Integer iaC(2,10)
      Integer iaCoolant(2,10)
      Integer iaHp(2,10)
      Integer iaStat(2)

*
* Set up for screen management
*

* Playfield
      J=iGrid*2+1
      I=iGrid+1
      Call SMG$CREATE_VIRTUAL_DISPLAY(I,J,iPlay,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iPlay,'War Field '//CHAR(iLevel+48))
      Call CreateGrid
      Call DisplayGrid
      I=(20-iGrid)/2+2
      J=I*2-1
      Call SMG$PASTE_VIRTUAL_DISPLAY(iPlay,iPaste,I,J)

* Players
      Do J=1,2
        iaTank(J)=ivTank
        I=1
        Do While (i.LE.iaTank(J))
          iR=iRnd(1,iGrid)
          iC=iRnd(1,iGrid)
          if (cGrid(iR,iC).EQ.' ') Then
            If (J.EQ.1) then
              cGrid(iR,iC)='#'
            Else
              cGrid(iR,iC)='%'
            End If
            iaCoolant(J,i)=100
            iaHp(J,i)=4
            iaR(J,i)=iR
            iaC(J,i)=iC
            I=I+1
          End If
        End Do
      End Do

* Player Stats
      Call SMG$CREATE_VIRTUAL_DISPLAY(5,30,iPs,SMG$M_BORDER)
      iaStat(1)=iPs
      Call SMG$LABEL_BORDER(iPs,'Player 1 (#) Stats')

      Call SMG$CREATE_VIRTUAL_DISPLAY(5,30,iPs,SMG$M_BORDER)
      iaStat(2)=iPs
      Call SMG$LABEL_BORDER(iPs,'Player 2 (%) Stats')
 
      Do I=1,2
        iPs=iaStat(I)
        iR=(I-1)*7+2
        Call SMG$PASTE_VIRTUAL_DISPLAY(iPs,iPaste,iR,47)
      End Do

* Command Display
      Call SMG$CREATE_VIRTUAL_DISPLAY(7,30,iHelp,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iHelp,'Commands:')
      Call SMG$HOME_CURSOR(iHelp,0)
      Call SMG$PUT_LINE(iHelp,'  Key    What it does')
      Call SMG$PUT_LINE(iHelp,'------  --------------')
      Call SMG$PUT_LINE(iHelp,'   0    Fire from tank')
      Call SMG$PUT_LINE(iHelp,'   5    Move tank')
      Call SMG$PUT_LINE(iHelp,'   q    Quit to Title')
      Call SMG$PUT_LINE(iHelp,'   x    Xit to VMS')
      Call SMG$PUT_LINE(iHelp,'   ?    Help')
      Call SMG$PASTE_VIRTUAL_DISPLAY(iHelp,iPaste,16,47)

      Do iPlayer=1,2
        Call DoStats(iPlayer,1)
      End Do
      iPlayer=1
      iKey=0
      Call UpdateGrid
      Do While (iPlayer.GT.0)
        iTank=1
        Call Boombox('   Player '//CHAR(48+iPlayer)//'   ')
        Do While (iTank.LE.iaTank(iPlayer))
          iPs=iaStat(iPlayer)
          Call DoStats(iPlayer,iTank)
          iR=iaR(iPlayer,iTank)
          iC=iaC(iPlayer,iTank)
          icR=iR
          icC=iC*2
* Get Key stroke and Parse...
          Call SMG$CHANGE_RENDITION(iPlay,icR,icC,1,1,SMG$M_REVERSE
     -                              + SMG$M_BOLD)
          iKey=iGetKey()
* Parsing Shoot
          If (iKey.EQ.48) then
            Call Shoot(iPlayer,iTank)
          End If
* Parsing movement
          If (iKey.EQ.53) then
            Call Move(iPlayer,iTank)
          End If
          Call SMG$CHANGE_RENDITION(iPlay,icR,icC,1,1,SMG$M_NORMAL)
          if (iKey.EQ.63) Call HelpMe
          If ((iKey.EQ.120).OR.(iKey.EQ.88)) Call ExitWin
          If (iKey.EQ.18) Call SMG$REPAINT_SCREEN(iPaste)
          If ((ikey.EQ.81).OR.(ikey.EQ.113)) Then
            iDoLevel=0
            Goto 999
          End If
          Call UpdateGrid
        End Do
        iTank=1
        Do While (iTank.LE.iaTank(iPlayer))
          If (iaCoolant(iPlayer,iTank).LT.13) Then
            Call MeltDown(iPlayer,iTank)
          End If
          iTank=iTank+1
        End Do
        iPs=iPlayer
        if (iPs.EQ.1) iPlayer=2
        if (iPs.EQ.2) iPlayer=1
        If ((iaTank(1).EQ.0).OR.(iaTank(2).EQ.0)) iPlayer=0
      End Do

      Call BoomBox('   Game Over   ')

      If (iaTank(1).EQ.0) Then
        iDoLevel=2
      Else
        iDoLevel=1
      End If

 999  Call SMG$POP_VIRTUAL_DISPLAY(iPlay,iPaste)
      Return
      End

* Subroutine to initalize all constants for the level
      Subroutine GetInfo(iLevel)
      Common  /VARS/iGun,iPits,iMines,iHills,ivTank,lOmni
      Logical lOmni

      Common  /VIDEO/iPaste,iKeybd,iPlay
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Character Ln(8)*52
* Display title graphics
      Ln(1)='         ___                  ______'
      Ln(2)='        /        /              /          /'
      Ln(3)='       /    / / /__  /_\\  /_   /  __/ /_  /_/'
      Ln(4)='       ---  \\/ /__/  \\_  /    /  /_/ / / /  \\'
      Ln(5)='           _/'
      Ln(6)=' '
      Ln(7)=' (c) 1992 McMullan Information Network Enterprises'
      Ln(8)='  &  The Scrambler (A.k.A.  David Rudy)'
      Call SMG$CREATE_VIRTUAL_DISPLAY(10,52,iTitle,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iTitle,'Select Level')
      Call SMG$HOME_CURSOR(iTitle,0)
      Do I=1,8
        Call SMG$PUT_LINE(iTitle,Ln(I))
      End Do
      Call SMG$PASTE_VIRTUAL_DISPLAY(iTitle,iPaste,3,14)
      iLevel=0
      Do While ((iLevel.LT.1).OR.(iLevel.GT.7))
        Call SMG$PUT_CHARS(iTitle,'Level 1-7: ',10,1,1,SMG$M_FORWARD)
        iKey=iGetKey()
        iLevel=iKey-48
      End Do
      If (iLevel.EQ.1) Then
        iGrid=10
        iGun=3
        ivTank=iRnd(3,5)
        iPits=0
        iMines=0
        iHills=0
      End If
      If (iLevel.EQ.2) Then
        iGrid=12
        iGun=3
        ivTank=iRnd(3,6)
        iPits=0
        iMines=iRnd(2,4)
        iHills=0
      End If
      If (iLevel.EQ.3) Then
        iGrid=15
        iGun=3
        ivTank=iRnd(3,6)
        iPits=0
        iMines=iRnd(5,8)
        iHills=4
      End If
      If (iLevel.EQ.4) Then
        iGrid=15
        iGun=3
        ivTank=iRnd(3,7)
        iPits=4
        iMines=iRnd(5,8)
        iHills=4
      End If
      If (iLevel.EQ.5) Then
        iGrid=15
        iGun=3
        ivTank=iRnd(3,7)
        iPits=6
        iMines=iRnd(6,10)
        iHills=6
      End If
      If (iLevel.EQ.6) Then
        iGrid=15
        iGun=3
        ivTank=iRnd(3,8)
        iPits=6
        iMines=iRnd(6,10)
        iHills=6
      End If
      If (iLevel.EQ.7) Then
        iGrid=20
        iGun=5
        ivTank=iRnd(3,8)
        iPits=6
        iMines=iRnd(6,15)
        iHills=8
      End If
      Call SMG$POP_VIRTUAL_DISPLAY(iTitle,iPaste)
      Call SMG$ERASE_PASTEBOARD(iPaste)
      Call SMG$REPAINT_SCREEN(iPaste)
      Return
      End

* Subroutine that handles Phone/Mail messages
      Subroutine BroadAST()
      Common  /VIDEO/iPaste,iKeybd,iPlay
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Character cMess*132
      Call SMG$GET_BROADCAST_MESSAGE(iPaste,cMess,iLen)
      If (iLen.GT.78) iLen=78
      Call SMG$CREATE_VIRTUAL_DISPLAY(3,iLen,iMess,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iMess,'VMS Interrupt')
      Call SMG$PUT_LINE(iMess,' ')
      Call SMG$PUT_LINE(iMess,cMess)
      iPut=(78-iLen)/2+1
      Call SMG$PASTE_VIRTUAL_DISPLAY(iMess,iPaste,10,iPut)
      Call SMG$RING_BELL(iMess,1)
      fS=SECNDS(0.0)
      Do While (SECNDS(fS).LT.2)
      End Do
      Call SMG$POP_VIRTUAL_DISPLAY(iMess,iPaste)
      Return
      End

* Subroutine that handles Out-of-band control characters
      Subroutine OutAST()
      Common  /VIDEO/iPaste,iKeybd,iPlay
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Call SMG$CREATE_VIRTUAL_DISPLAY(3,20,iMess,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iMess,'Keybd Interrupt')
      Call SMG$PUT_LINE(iMess,' ')
      Call SMG$PUT_LINE(iMess,' Non-Implemented Key')
      Call SMG$PASTE_VIRTUAL_DISPLAY(iMess,iPaste,10,29)
      Call SMG$RING_BELL(iMess,1)
      fS=SECNDS(0.0)
      Do While (SECNDS(fS).LT.1)
      End Do
      Call SMG$POP_VIRTUAL_DISPLAY(iMess,iPaste)
      Return
      End

* Appendation Function
      Subroutine Append(cOut,cIn)
      Character cOut(30),cIn(10)
      I=1
      Do While (cIn(I).EQ.' ')
        I=I+1
      End Do
      J=25
      Do While (cOut(J).EQ.' ')
        J=J-1
      End Do
      J=J+2
      Do K=I,10
        cOut(J+K-I)=cIn(K)
      End Do
      Do I=1,10
        cIn(I)=' '
      End Do
      Return
      End        

* Put the stats into a virtual pasteboard
      Subroutine DoStats(iPlayer,iTank)
      Include '($SMGDEF)'
      Common  /ARRAYS/iaTank,iaR,iaC,iaCoolant,iaHp,iaStat
      Integer iaTank(2)
      Integer iaR(2,10)
      Integer iaC(2,10)
      Integer iaCoolant(2,10)
      Integer iaHp(2,10)
      Integer iaStat(2)

      Character cOutl*30,cOutv*10

      iR=iaR(iPlayer,iTank)
      iC=iaC(iPlayer,iTank)
      iCool=iaCoolant(iPlayer,iTank)
      iHp=iaHp(iPlayer,iTank)
      iTanks=iaTank(iPlayer)
      iPs=iaStat(iPlayer)

      Call SMG$HOME_CURSOR(iPs,0)
      Call STR$COPY_DX(cOutl,'Tanks: ')
      Call OTS$CVT_L_TU(iTanks,cOutv)
      Call Append(cOutl,cOutv)
      Call SMG$PUT_LINE(iPs,cOutl)

      Call STR$COPY_DX(cOutl,'Tank: ')
      Call OTS$CVT_L_TU(iTank,cOutv)
      Call Append(cOutl,cOutv)
      cOutv='@ ('
      Call Append(cOutl,cOutv)
      cOutv=char(iR+64)
      Call Append(cOutl,cOutv)
      cOutv=','
      Call Append(cOutl,cOutv)
      Call OTS$CVT_L_TU(iC,cOutv)
      Call Append(cOutl,cOutv)
      cOutv=')'
      Call Append(cOutl,cOutv)
      Call SMG$PUT_LINE(iPs,cOutl)

      Call STR$COPY_DX(cOutl,'Coolant: ')
      Call OTS$CVT_L_TU(iCool,cOutv)
      Call Append(cOutl,cOutv)
      cOutv='%'
      Call Append(cOutl,cOutv)
      Call SMG$PUT_LINE(iPs,cOutl)

      Call STR$COPY_DX(cOutl,'     HP: ')
      Call OTS$CVT_L_TU(iHp,cOutv)
      Call Append(cOutl,cOutv)
      Call SMG$PUT_LINE(iPs,cOutl)

      If ((iCool.LT.25).or.(iHp.LE.1)) Then
        Call SMG$PUT_LINE(iPs,'[CRITICAL]',,SMG$M_REVERSE
     -                    + SMG$M_BLINK)
      Else
        Call SMG$PUT_LINE(iPs,' ')
      End If
      Return
      End

* Subroutine that Redraws the Playfield
      Subroutine UpdateGrid
      Common  /VARS/iGun,iPits,iMines,iHills,ivTank,lOmni
      Logical lOmni

      Common  /VIDEO/iPaste,iKeybd,iPlay
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Character cOuts,cTmp
      Call SMG$HOME_CURSOR(iPlay,0)
      iPits=0
      iMines=0
      iHills=0
      Do iR=1,iGrid
        Do iC=1,iGrid
          cTmp=cGrid(iR,iC)
          If (cTmp.EQ.'O') iPits=iPits+1
          If (cTmp.EQ.'^') iHills=iHills+1
          If (cTmp.EQ.'_') Then
            cTmp=' '
            iMines=iMines+1
          End If
          If (cTmp.NE.oGrid(iR,iC)) Then
            cOuts=cTmp
            inC=iC*2
            Call SMG$PUT_CHARS(iPlay,cOuts,iR,inC,SMG$M_FORWARD)
            oGrid(iR,iC)=cTmp
          End If
        End Do
      End Do
      Call SMG$HOME_CURSOR(iPlay,1)
      Return
      End

* Superior Random Function
      Function iRnd(iLow,iUp)
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      fDiff=iUp-iLow+1.0  
      iRnd=fDiff*Ran(iSeed)+iLow
      Return
      END 

* Subroutine that created the Landscape      
      Subroutine CreateGrid
      Common  /VARS/iGun,iPits,iMines,iHills,ivTank,lOmni
      Logical lOmni

      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Do iR=1,iGrid
        Do iC=1,iGrid
          cGrid(iR,iC)=' '
        End Do
      End Do
      I=1
      Do While (i.LE.iHills)
        iR=iRnd(2,iGrid-1)
        iC=iRnd(2,iGrid-1)
        if (cGrid(iR,iC).EQ.' ') Then
          Do J=1,8
            iRi=iRnd(-1,1)
            iCi=iRnd(-1,1)
            cGrid(iR+iRi,iC+iCi)='^'
          End Do
          I=I+1
        End If
      End Do
      I=1
      Do While (i.LE.iMines)
        iR=iRnd(1,iGrid)
        iC=iRnd(1,iGrid)
        if (cGrid(iR,iC).EQ.' ') Then
          cGrid(iR,iC)='_'
          I=I+1
        End If
      End Do
      I=1
      Do While (i.LE.iPits)
        iR=iRnd(1,iGrid)
        iC=iRnd(1,iGrid)
        if (cGrid(iR,iC).EQ.' ') Then
          cGrid(iR,iC)='O'
          I=I+1
        End If
      End Do
      Return
      End

* Subroutine to Diplay Grid in entirety.
      Subroutine DisplayGrid
      Common  /VIDEO/iPaste,iKeybd,iPlay
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Include '($SMGDEF)'
      Do iR=1,iGrid
        Call SMG$PUT_CHARS(iPlay,Char(iR+64),iR,1,,SMG$M_REVERSE)
      End Do
      iR=iGrid+1
      Call SMG$PUT_CHARS(iPlay,
     -     ' 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 ',
     -     iR,1,,SMG$M_REVERSE)
      Call UpdateGrid
      Return
      End
  
* Subroutine to exit from the game instantly.
      Subroutine ExitWin
      Common  /SGM/iPaste,iKeybd
      Call SMG$DELETE_PASTEBOARD(iPaste)
      Call SMG$DELETE_VIRTUAL_KEYBOARD(iKeybd)
      Stop
      End

* Box to Display death status...
      Subroutine BoomBox(cLabel)
      Include '($SMGDEF)'
      Common  /SGM/iPaste,iKeybd
      Character cLabel*(*)
      iPut=len(cLabel)
      Call SMG$CREATE_VIRTUAL_DISPLAY(1,iPut,iBoom,SMG$M_BORDER)
      Call SMG$PUT_LINE(iBoom,cLabel)
      iPut=(78-len(cLabel))/2+1
      Call SMG$PASTE_VIRTUAL_DISPLAY(iBoom,iPaste,10,iPut)
      fS=SECNDS(0.0)
      Do While (SECNDS(fS).LT.1.5)
      End Do
      Call SMG$POP_VIRTUAL_DISPLAY(iBoom,iPaste)
      Return
      End

* Function to find a tank
      Function iFindtank(iPlayer,iR,iC)
      Common  /ARRAYS/iaTank,iaR,iaC,iaCoolant,iaHp,iaStat
      Integer iaTank(2)
      Integer iaR(2,10)
      Integer iaC(2,10)
      Integer iaCoolant(2,10)
      Integer iaHp(2,10)
      Integer iaStat(2)

      iTank=1
      Do While (.NOT.((iR.EQ.iaR(iPlayer,iTank)).AND.
     -                (iC.EQ.iaC(iPlayer,iTank))))
        iTank=iTank+1
      End Do
      iFindTank=iTank
      Return
      End 

* Delete a tank from the battlefield
      Subroutine DelTank(iPlayer,iTank,cLabel)
      Common  /VIDEO/iPaste,iKeybd,iPlay
      Character cLabel*(*)
      Common  /ARRAYS/iaTank,iaR,iaC,iaCoolant,iaHp,iaStat
      Integer iaTank(2)
      Integer iaR(2,10)
      Integer iaC(2,10)
      Integer iaCoolant(2,10)
      Integer iaHp(2,10)
      Integer iaStat(2)

      Call SMG$RING_BELL(iMess,5)
      Call BoomBox(cLabel)
      Call NukeTank(iaR(iPlayer,iTank),iaC(iPlayer,iTank))

      iaTank(iPlayer)=iaTank(iPlayer)-1
      Do J=iTank,iaTank(iPlayer)
        iaR(iPlayer,J)=iaR(iPlayer,J+1)
        iaC(iPlayer,J)=iaC(iPlayer,J+1)
        iaCoolant(iPlayer,J)=iaCoolant(iPlayer,J+1)
        iaHp(iPlayer,J)=iaHp(iPlayer,J+1)
      End Do
      Return
      End

* Function that gets the direction for Fire/Move
      Function iGetdir(cLabel,inR,inC,lOmni)
      Common  /VIDEO/iPaste,iKeybd,iPlay
      Logical lOkey,lOmni
      Character cLabel*(*),Ln(5)*8
      Ln(1)=' 7  8  9'
      Ln(2)=' '
      Ln(3)=' 4     6'
      Ln(4)=' '
      Ln(5)=' 1  2  3'
      Call SMG$CREATE_VIRTUAL_DISPLAY(5,8,iDir,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iDir,cLabel)
      Call SMG$HOME_CURSOR(iDir,0)
      Do i=1,5
        Call SMG$PUT_LINE(iDir,Ln(i))
      End Do
      Call SMG$DRAW_LINE(iDir,1,3,5,3)
      Call SMG$DRAW_LINE(iDir,1,6,5,6)
      Call SMG$DRAW_LINE(iDir,2,1,2,8)
      Call SMG$DRAW_LINE(iDir,4,1,4,8)
      Call SMG$PASTE_VIRTUAL_DISPLAY(iDir,iPaste,5,65)
      lOkey=.FALSE.
      Do While (.NOT.lOkey)
        iKey=iGetKey()
        I=iKey-48
        If (((I.LT.10).AND.(I.GE.0)).OR.(I.LT.0)) Then
          lOkey=((I.GE.1).AND.(I.LE.9).AND.(I.NE.5)).OR.(I.LT.0)
        End If
      End Do
      inC=0
      inR=0
      If ((I.EQ.1).OR.(I.EQ.4).OR.(I.EQ.7)) inC=-1
      If ((I.EQ.3).OR.(I.EQ.6).OR.(I.EQ.9)) inC=1
      If ((I.EQ.7).OR.(I.EQ.8).OR.(I.EQ.9)) inR=-1
      If ((I.EQ.1).OR.(I.EQ.2).OR.(I.EQ.3)) inR=1
      if (I.LT.0) then
        iGetDir=-1
      Else
        iGetDir=1
      End If
      Call SMG$POP_VIRTUAL_DISPLAY(iDir,iPaste)
      Return 
      End

* Moves the tank
      Subroutine Move(iPlayer,iTank)
      Common  /VARS/iGun,iPits,iMines,iHills,ivTank,lOmni
      Logical lOmni

      Common  /VIDEO/iPaste,iKeybd,iPlay
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Character cSpot

      Common  /ARRAYS/iaTank,iaR,iaC,iaCoolant,iaHp,iaStat
      Integer iaTank(2)
      Integer iaR(2,10)
      Integer iaC(2,10)
      Integer iaCoolant(2,10)
      Integer iaHp(2,10)
      Integer iaStat(2)

      Logical lDone

      Integer iMoveHeat
      Integer iHillHeat
      Integer iMeBump
      Integer iYouBump

      iMoveHeat=4
      iHillHeat=3
      iMeBump=1
      iYouBump=15

      iR=iaR(iPlayer,iTank)
      iC=iaC(iPlayer,iTank)
      If (iGetdir('Movement',inR,inC,lOmni).GT.0) Then
        inR=iR+inR
        inC=iC+inC
        If ((inR.GT.0).And.(inC.GT.0).And.(inR.LE.iGrid).And.
     -      (inC.LE.iGrid)) Then
          iaCoolant(iPlayer,iTank)=iaCoolant(iPlayer,iTank)-iMoveHeat
          cSpot=cGrid(inR,inC)
*   If We hit a mine...
          If (cSpot.EQ.'_') Then
            iaR(iPlayer,iTank)=inR
            iaC(iPlayer,iTank)=inC
            cGrid(iR,iC)=' '
            Call DelTank(iPlayer,iTank,'Mine')
            cGrid(inR,inC)=' '
          End If
* If we hit a Pit...
          If (cSpot.EQ.'O') then
            iaR(iPlayer,iTank)=inR
            iaC(iPlayer,iTank)=inC
            cGrid(iR,iC)=' '
            Call DelTank(iPlayer,iTank,'Pit')
            cGrid(inR,inC)='O'
          End If
* If we hit another tank or a hill....
          If ((cSpot.EQ.'#').OR.(cSpot.EQ.'%').OR.(cSpot.EQ.'^')) Then
            If (cSpot.EQ.'^') Then
              iHeat=iHillHeat
              Call BoomBox('Hill')
            Else
              Call BoomBox('BUMP!')
              iHeat=iMeBump
            End If
            iaHp(iPlayer,iTank)=iaHp(iPlayer,iTank)-1
            Call DoStats(iPlayer,iTank)
            iaCoolant(iPlayer,iTank)=iaCoolant(iPlayer,iTank)-iHeat
            If (iaHp(iPlayer,iTank).LE.0) Then
              Call DelTank(iPlayer,iTank,'Zero HP')
              cGrid(iR,iC)=' '
            Else
              iTank=iTank+1
            End If
            If (cSpot.NE.'^') Then
              if (cSpot.EQ.'#') then
                iP=1
              Else
                iP=2
              End If
              iTnk=iFindTank(iP,inR,inC)
              iaHp(iP,iTnk)=iaHp(iP,iTnk)-1
              Call DoStats(iP,iTnk)
              iaCoolant(iP,iTnk)=iaCoolant(iP,iTnk)-iYouBump
              If (iaHp(iP,iTnk).LE.0) Then
                Call DelTank(iP,iTnk,'Zero HP')
                cGrid(inR,inC)=' '
              End If
            End If
          End If
* if we hit blank space or a dead tank...
          If (cSpot.EQ.' ') then
            if (iPlayer.EQ.1) then
              cGrid(inR,inC)='#'
            else
              cGrid(inR,inC)='%'
            End If
            iaR(iPlayer,iTank)=inR
            iaC(iPlayer,iTank)=inC
            cGrid(iR,iC)=' '
            iTank=iTank+1
          End If
        End If
      End If
      Return
      End
 
* Shoots the other tank, destroys stuff in line-of fire
      Subroutine Shoot(iPlayer,iTank)
      Common  /VARS/iGun,iPits,iMines,iHills,ivTank,lOmni
      Logical lOmni

      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Character cTmp

      Common  /ARRAYS/iaTank,iaR,iaC,iaCoolant,iaHp,iaStat
      Integer iaTank(2)
      Integer iaR(2,10)
      Integer iaC(2,10)
      Integer iaCoolant(2,10)
      Integer iaHp(2,10)
      Integer iaStat(2)

      Logical lDone

      Integer iShootHeat
      Integer iDamageHeat

      iShootHeat=6
      iDamageHeat=10

      iR=iaR(iPlayer,iTank)
      iC=iaC(iPlayer,iTank)
      If (iGetdir('Fire',inR,inC,lOmni).GT.0) then
        iaCoolant(iPlayer,iTank)=iaCoolant(iPlayer,iTank)-iShootHeat
        itR=iR+inR
        itC=iC+inC
        iTank=iTank+1
        lDone=((itC.LT.1).OR.(itC.GT.iGrid).OR.
     -         (itR.GT.iGrid).OR.(itR.LT.1).OR.
     -         (ABS(itR-iR).GT.iGun).OR.(ABS(itC-iC).GT.iGun))
        Do While ((.NOT.lDone).AND.(cGrid(itR,itC).NE.'^'))
          cTmp=cGrid(itR,itC)
          cGrid(itR,itC)='+'
          Call UpdateGrid
          I=iRnd(1,10)
          If (((cTmp.Eq.'#').OR.(cTmp.EQ.'%')).AND.(I.LE.6)) Then
            If (cTmp.EQ.'#') Then
              iPl=1
            Else
              iPl=2
            End If
            iTnk=iFindTank(iPl,itR,itC)
            iaHp(iPl,iTnk)=iaHp(iPl,iTnk)-1
            Call DoStats(iPl,iTnk)
            iaCoolant(iPl,iTnk)=iaCoolant(iPl,iTnk)-iDamageHeat
            If (iaHp(iPl,iTnk).LE.0) Then
              Call DelTank(iPl,iTnk,'Destroyed')
              cTmp=' '
            Else
              Call BoomBox('Hit')
            End If
            lDone=.TRUE.
          End if
          cGrid(itR,itC)=cTmp
          Call UpdateGrid
          itR=itR+inR
          itC=itC+inC
          lDone=(lDone.OR.
     -          (itC.LT.1).OR.(itC.GT.iGrid).OR.
     -          (itR.GT.iGrid).OR.(itR.LT.1).OR.
     -          (ABS(itR-iR).GT.iGun).OR.(ABS(itC-iC).GT.iGun))
        End Do
      End If
      Return
      End

* Nukes Tank on the battlefield
      Subroutine NukeTank(iR,iC)
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Character cIn(4)
      cIn(1)='-'
      cIn(2)='\\'
      cIn(3)='|'
      cIn(4)='/'
      Do iCells=1,10
        Do iFrame=1,4
          cGrid(iR,iC)=cIn(iframe)
          Call UpdateGrid
        End Do
      End Do
      Return
      End

* Reactor Melt Down Subroutine
      Subroutine Meltdown(iPlayer,iTank)
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)

      Common  /ARRAYS/iaTank,iaR,iaC,iaCoolant,iaHp,iaStat
      Integer iaTank(2)
      Integer iaR(2,10)
      Integer iaC(2,10)
      Integer iaCoolant(2,10)
      Integer iaHp(2,10)
      Integer iaStat(2)

      iR=iaR(iPlayer,iTank)
      iC=iaC(iPlayer,iTank)
      Call DoStats(iPlayer,iTank)
      Call DelTank(iPlayer,iTank,'Reactor Meltdown')
      iTnk=iTank
      Do I=0,2
      Do inR=iR-I,iR+I
        iAdd=I-ABS(inR-iR)
        Do inC=iC-iAdd,iC+iAdd
          If ((inR.GE.1).AND.(inR.LE.iGrid).AND.(inC.GE.1).AND.
     -        (inC.LE.iGrid)) Then
            If ((cGrid(inR,inC).EQ.'#').OR.(cGrid(inR,inC).EQ.'%')) Then
              If (cGrid(inR,inC).EQ.'#') then
                iPl=1
              Else
                iPl=2
              End If
              iTnk=iFindTank(iPl,inR,inC)
              Call DelTank(iPl,iTnk,'Blast Wave')
            End If
          End If
          cGrid(inR,inC)='O'
          Call UpdateGrid
        End Do
      End Do
      End Do
      Return
      End

* Help subroutine for the main program
      Subroutine HelpMe
      Common  /VARS/iGun,iPits,iMines,iHills,ivTank,lOmni
      Logical lOmni

      Common  /VIDEO/iPaste,iKeybd,iPlay
      Common  /DATA/iSeed,iGrid,cGrid,oGrid
      Character cGrid(20,20),oGrid(20,20)
      Character Ln(8)*57
      Call SMG$CREATE_VIRTUAL_DISPLAY(10,57,iHelp,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iHelp,'CyberTank Help')
      Call SMG$HOME_CURSOR(iHelp,0)
      Ln(1)='  ( ) Mines: '//CHAR(48+(iMines/10))
     -                     //CHAR(48+MOD(iMines,10))
      Ln(2)='  (O)  Pits: '//CHAR(48+(iPits/10))//CHAR(48+MOD(iPits,10))
      Ln(3)='  (^) Hills: '//CHAR(48+(iHills/10))
     -                     //CHAR(48+MOD(iHills,10))
      Ln(4)=' '
      Ln(5)='     Cybertanks are matter/antimatter fueled vehicles'
      Ln(6)=' so they require coolant.  Unfortunately, the coolant'
      Ln(7)=' evaporates on use, until it reaches 13%.   Then, the'
      Ln(8)=' matter/antimatter pods fail and explode.'
      Do I=1,8
        Call SMG$PUT_LINE(iHelp,Ln(I))
      End Do
      Call SMG$PUT_LINE(iHelp,' ')
      Call SMG$PUT_LINE(iHelp,' Press [RETURN] for next page.')
      Call SMG$PASTE_VIRTUAL_DISPLAY(iHelp,iPaste,6,11)
      iKey=iGetKey()
      If (iKey.NE.13) Goto 999
      Call SMG$POP_VIRTUAL_DISPLAY(iHelp,iPaste)
      Call SMG$CREATE_VIRTUAL_DISPLAY(10,40,iHelp,SMG$M_BORDER)
      Call SMG$LABEL_BORDER(iHelp,'CyberTank Help')
      Call SMG$HOME_CURSOR(iHelp,0)
      Ln(1)='Coolant/Hp Loss Table: '
      Ln(2)='        Your Tank     Opponent''s Tank'
      Ln(3)='       -----------   ------------------'
      Ln(4)=' Moving:   4'
      Ln(5)=' Shots:    6          If Missed: 0'
      Ln(6)='                            Hit: 10,1Hp'
      Ln(7)=' Ramming:  5,1Hp                 15,1Hp'
      Ln(8)='   Hills:  7,1Hp'
      Do I=1,8
        Call SMG$PUT_LINE(iHelp,Ln(I))
      End Do
      Call SMG$PUT_LINE(iHelp,' ')
      Call SMG$PUT_LINE(iHelp,' Press [RETURN] for next page.')
      Call SMG$PASTE_VIRTUAL_DISPLAY(iHelp,iPaste,6,19)
      iKey=iGetKey()
 999  Call SMG$POP_VIRTUAL_DISPLAY(iHelp,iPaste)
      Return 
      End

