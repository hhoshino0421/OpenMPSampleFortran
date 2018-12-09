! ============================================================================
! Name        : TestApp.f90
! Author      : Hoshino Hitoshi
! Version     :
! Copyright   : Your copyright notice
! Description : OpenMP Test Application
! ============================================================================

program TestApp
    implicit none

! 変数宣言
integer i
integer j
integer k
integer::n = 10000
double precision,dimension(10000,10000) :: data1
double precision,dimension(10000,10000) :: data2
double precision,dimension(10000,10000) :: data3

!character*10 date, time, zone
!integer values(8)
integer t1, t2, diff
integer t_rate, t_max


print "(A)","Start Program"

!call date_and_time(date, time, zone, values)
!print *,date
!print *,time
!print *,zone
!print *,values
! 開始時刻取得
call system_clock(t1)

! print *,t1


! データ準備
! ここから並列実行
!$omp parallel do
do i=1,n,1
    do j=1,n,1

        call random_number(data1(i,j))
        data1(i,j) = data1(i,j) * 1000

        call random_number(data2(i,j))
        data2(i,j) = data2(i,j) * 10000

        data3(i,j) = 0.0

    enddo

enddo
!$omp end parallel do
! ここまで並列実行

! 行列の積を計算
! ここから並列実行
!$omp parallel do
do i=1, n ,1
    do j=1,n,1
        do k=1,n,1

            data3(i,j) = data1(i,k) * data2(k,j)

        enddo

    enddo

enddo
!$omp end parallel do
! ここまで並列実行

!do i=1, n ,1
!    do j=1,n,1
!
!        print *, data3(i,j)
!
!    enddo
!enddo

! 処理時間計算
call system_clock(t2, t_rate, t_max)   ! 終了時を記録

if ( t2 < t1 ) then
    diff = (t_max - t1) + t2 + 1
else
    diff = t2 - t1
endif

print "(A,I10)","t1:", t1
print "(A,I10)","t2:", t2
print "(A, F10.4)", "time it took was:", diff/dble(t_rate)

print "(A)","End Program"

end program
