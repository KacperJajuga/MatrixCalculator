! operacje_macierzowe
!Kacper Jajuga, kierunek: informatyka, rok I, semestr II
	program operacje_macierzowe
    character*15 nazwa1, nazwa2, nazwa3
    character*1 tn
    dimension a(20,20),b(20,20),c(20,20),d(20,20)
    real alfa, pomocnicza
! 
100	write(*,*) 'KALKULATOR MACIERZOWY'
    write(*,'(10x,a)')'10 - dodawanie  a i b wynik w c' !tutaj podac nazwy pliku a2x3 b2x3
    write(*,'(10x,a)')'20 - porownanie a i b ' !tutaj podac nazwy pliku a2x3 b2x3
    write(*,'(10x,a)')'30 - mnozenie a przez liczbe alfa wynik w c' !tutaj podac nazwy pliku a2x3 b2x3
    write(*,'(10x,a)')'40 - mnozenie a przez b wynik w c' !tutaj podac nazwy pliku a2x3 b3x2
    write(*,'(10x,a)')'50 - transpozycja a  ' !tutaj podac nazwy pliku a2x3 b2x3
    write(*,'(10x,a)')'60 - rozklad a na czesc symetryczna c i asymetryczna d ' !tutaj podac nazwy pliku a2x2 b2x3
    write(*,'(10x,a)')'70 - koniec dzialania aplikacji  '
    write(*,*)' Podaj numer operacji iet: '
    read(*,*)iet 
    if(iet.eq.70)goto 70
    write(*,'(a,i3,a)')' operacja nr', iet, '   nacisnij ENTER'
    read(*,*)
    
1	write(*,*)' Podaj nazwy plikow a i b (max. 15 znakow)   :'
	read(*,*)nazwa1, nazwa2
	open(10,FILE=nazwa1,STATUS='old')
    open(11,FILE=nazwa2,STATUS='old')

! czytanie z plikow  macierzy a i b
    write(*,'(a\)')' Podaj rozmiar  macierzy a( n1 wierszy x m1 kolumn )  : '
    read(*,*)n1,m1
     write(*,'(a\)')' Podaj rozmiar  macierzy b( n2 wierszy x m2 kolumn )  : '
    read(*,*)n2,m2
    write(*,*)
    do i=1,n1
      read(10,*)(a(i,j),j=1,m1)
      write(*,'(20f6.1)')(a(i,j),j=1,m1)
    enddo !i
    write(*,*)
    do i=1,n2
      read(11,*)(b(i,j),j=1,m2)
      write(*,'(20f6.1)')(b(i,j),j=1,m2) 
    enddo !i
    write(*,*)
    close(10)
    close(11)
   
	if(iet.eq.10)goto 10
    if(iet.eq.20)goto 20
    if(iet.eq.30)goto 30
    if(iet.eq.40)goto 40
    if(iet.eq.50)goto 50
    if(iet.eq.60)goto 60
    
    
! dodawanie macierzy b do a i zapisanie wyniku w macierzy c
10	write(*,*)' Dodawanie macierzy b do a i zapisanie wyniku w macierzy c. '
! porownanie rozmiarow macierzy a i b
	if((n1.eq.n2).and.(m1.eq.m2))then
    n=n1
    m=m1
    else
    write(*,*)' macierze sa roznych rozmiarow, NIE MOZNA dodawac'
    goto 2
    endif
	do i=1,n
    do j=1,m
      c(i,j)=a(i,j)+b(i,j)
    enddo!j
    	write(*,'(20f6.1)')(c(i,j),j=1,m)
    enddo !i
    write(*,*)
    write(*,*) 'podaj nazwe pliku do zapisania wyniku dodawania   :'
    read(*,*) nazwa3
    open(10, file=nazwa3, status='unknown')
    do i=1,n
  		write(10,'(2x,20f6.1)')(c(i,j),j=1,m)
    	write(*,'(2x,20f6.1)')(c(i,j),j=1,m)    
    enddo !n
!
	close(10)
    write(*,*)' Nacisnij klawisz ENTER'
    read(*,*)
!
2	write(*,*)' Czy koniec dodawania (t lub n)?  '
    read(*,*)tn
    if(tn.eq.'n')goto 1
    goto 100
    
! porownanie macierzy a i b
20	write(*,*)' Porownanie macierzy a i b. '
	if((n1.ne.n2).or.(m1.ne.m2))then
    write(*,*)
	write(*,*)' Macierze maja rozne rozmiary - nie mozna ich porownac. '
	tn='3'
    if(tn.eq.'3')goto 3
    endif
    iwsk=0
    n=n1
    m=m1
    do i=1,n
      do j=1,m     
		if(abs(a(i,j)-b(i,j)).gt.1.0e-6)then
   		write(*,'(a,i3,a,i3,a,i3,a,i3,a)')'Element a(',i,',',j,') rozny od elementu b(',i,',',j,')'
        iwsk=1
        endif
      enddo !j
    enddo !i
	if(iwsk.eq.0)write(*,*)' Macierze sa identyczne. '
    write(*,*)
3	write(*,*)' Czy koniec porownywania (t lub n)?  ' 
    read(*,*)tn
    if(tn.eq.'n')goto 1
    goto 100
   
! mnozenie macierz
30	write(*,*)' Mnozenie macierzy a przez liczbe alfa i zapisanie wyniku w d. '
    write(*,'(a\)') 'Podaj licze alfa: '
    read(*,*)alfa
    n=n1
    m=m1
    do i=1, n
      do j=1, m
        d(i,j)=a(i,j)*alfa
      enddo !j
      write(*,'(20f6.1)')(d(i,j),j=1,m)
    enddo !i
    write(*,*)
    write(*,*) 'podaj nazwe pliku do zapisania wyniku mnozenia macierzy przez liczbe: '
    read(*,*) nazwa3
    open(10, file=nazwa3, status='unknown')
    do i=1,n
  		write(10,'(2x,20f6.1)')(d(i,j),j=1,m)
    	write(*,'(2x,20f6.1)')(d(i,j),j=1,m)    
    enddo !n
!
	close(10)
    write(*,*)' Nacisnij klawisz ENTER'
    read(*,*)
!
4	write(*,*)' Czy koniec mnozenia (t lub n)?  '
    read(*,*)tn
    if(tn.eq.'n')goto 1
    goto 100

40	write(*,*)' Mnozenie macierzy a i b i zapisanie wyniku w d. ' 
    ! porownanie rozmiarow macierzy a i b
	if((n1.eq.m2).and.(m1.eq.n2))then
    n=n1
    m=m1
    else
    write(*,*)'rozmiary tych macierzy NIE pozwalaja na ich mnozenie.'
    goto 5
    endif
    pomocnicza = 0
    do i=1,n
    do j=1,n
    do k=1,m
      pomocnicza=((pomocnicza)+(a(i,k)*b(k,j)))
      d(i,j)=pomocnicza
    enddo!k
    enddo!j
    enddo!i
    write(*,*)
    write(*,*) 'podaj nazwe pliku do zapisania wyniku mnozenia dwoch macierzy: '
    read(*,*) nazwa3
    open(10, file=nazwa3, status='unknown')
    do i=1,n
  		write(10,'(2x,20f6.1)')(d(i,j),j=1,n)
    	write(*,'(2x,20f6.1)')(d(i,j),j=1,n)    
    enddo !n
!
	close(10)
    write(*,*)' Nacisnij klawisz ENTER'
    read(*,*)
!
5	write(*,*)' Czy koniec mnozenia (t lub n)?  '
    read(*,*)tn
    if(tn.eq.'n')goto 1
    goto 100

50	write(*,*)' Transpozycja macierzy a  '
    n=n1
    m=m1
    do i=1,n
      do j=1,m
        c(i,j)=a(i,j) !przepisywanie macierzy a do macierzy, zeby mozna ja bylo pozniej transponowac ponownie do a
      enddo!j
    enddo!i
    do i=1,n
      do j=1,m
        a(j,i)=c(i,j)
      enddo!j
    enddo!i
    write(*,*)
    write(*,*) 'podaj nazwe pliku do zapisania transponowanej macierzy: '
    read(*,*) nazwa3
    open(10, file=nazwa3, status='unknown')
    do i=1,m
  		write(10,'(2x,20f6.1)')(a(i,j),j=1,n)
    	write(*,'(2x,20f6.1)')(a(i,j),j=1,n)    
    enddo !n
!
	close(10)
    write(*,*)' Nacisnij klawisz ENTER'
    read(*,*)
!
6	write(*,*)' Czy koniec transponowania (t lub n)?  '
    read(*,*)tn
    if(tn.eq.'n')goto 1
    goto 100

60	write(*,*)' Rozklad a na czesc symetryczna c i asymetryczna d '
    if(n1.eq.m1)then
    n=n1
    m=m1
    else
    write(*,*)'to NIE jest macierz kwadratowa, nie mozna wiec jej rozlozyc na czesc symetryczna i antysymetryczna.'
    goto 7
    endif
    
    do i=1,n
      do j=1,m
        b(j,i)=a(i,j) !macierz b jest tutaj transponowana macierza a
      enddo!j
    enddo!i
    do i=1, n
      do j=1, m
        c(i,j)=a(i,j)+b(j,i)!tworzenie macierzy symetrycznej
      enddo!j
    enddo!i
    do i=1,n
      do j=1,m
        d(i,j)=a(i,j)-b(i,j)!tworzenie macierzy asymetrycznej
      enddo!j
    enddo!i
    write(*,*) 'podaj nazwe pliku do zapisania czesci symetrycznej macierzy: '
    read(*,*) nazwa3
    open(10, file=nazwa3, status='unknown')
    do i=1,m
  		write(10,'(2x,20f6.1)')(c(i,j),j=1,n)
    	write(*,'(2x,20f6.1)')(c(i,j),j=1,n)    
    enddo !n
    close(10)
    write(*,*) 'podaj nazwe pliku do zapisania czesci asymetrycznej macierzy: '
    read(*,*) nazwa3
    open(10, file=nazwa3, status='unknown')
    do i=1,m
  		write(10,'(2x,20f6.1)')(d(i,j),j=1,n)
    	write(*,'(2x,20f6.1)')(d(i,j),j=1,n)    
    enddo !n
    close(10)
    
	!
    write(*,*)' Nacisnij klawisz ENTER'
    read(*,*)
!
7	write(*,*)' Czy koniec rozkladania macierzy na czesc symetryczna i asymetryczna (t lub n)?  '
    read(*,*)tn
    if(tn.eq.'n')goto 1
    goto 100

70	stop
    end