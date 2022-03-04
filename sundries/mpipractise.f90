
!mpi 初始化、获取进程号、获取进程书、分配进程执行任务、结束释放
! program name
!     use mpi
!     implicit none
!     integer:: num,size
!     integer:: i,ierr
!     call MPI_INIT(ierr)
!     call MPI_COMM_RANK(MPI_COMM_WORLD, num,ierr) 
!     call MPI_COMM_SIZE(MPI_COMM_WORLD,size,ierr)
!     if (num==0) then
!         do i=0,10
!             write(*,'(1x,I2 ,$)') i
!         end do
!         print*,num
!     elseif (num==1) then
!         do i=100,110
!             write(*,'(1x,I3 ,$ )') i
!         end do
!         print*,num
!     elseif (num==2) then
!             print *, "Hi,TG is a homesome and intelligent boy!",'/',num
!     elseif (num==5) then
!             print *, "Hello!,I love this world" ,'/',num
!     else 
!             print *,size,num,'------------------------------------'
!     endif
!     call MPI_Finalize(ierr)
! end program name

! program main
!     use MPI
!     implicit none
!     integer*4::ierr,my_id,num_procs
!     call MPI_INIT ( ierr )
!     call MPI_COMM_RANK (MPI_COMM_WORLD, my_id,ierr)
!     call MPI_COMM_SIZE (MPI_COMM_WORLD, num_procs,ierr) !num_procs--number of process
!     write(*,'(1x,i2,a,i2)')my_id,'/',num_procs

!     call MPI_FINALIZE ( ierr )
! end program

! mpi_send  \  mpi_recv运用
! Program main 
!     use mpi
!     impli以尝试一下输出前后用fflush(stdout)刷新一下输出缓冲区，可能和printf的it none 
!     integer :: myid ,ierr,total
!     integer :: A(3,3),i,j
!     real(4) sum,sum2
!     call MPI_INIT(ierr) 
!     call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr) 
!     call MPI_COMM_SIZE(MPI_COMM_WORLD,total,ierr)
!     sum=0.0
!     sum2=10
!     if (myid==0) then 
!         A=1
!     else
!         A=2
!     end if 
!     do j=1,3
!         do i=1,3
!         sum=A(i,j)+sum
!         end do
!     end do
!     if(myid==1) then
!         print*,sum2,myid
!         call mpi_send(sum,1,mpi_real,0,99,MPI_COMM_WORLD,ierr)！ierr不可以少
!     end if
!     if(myid==0) then
!         sum2=222
!         print*,sum,sum2,myid
!         call mpi_recv(sum2,1,mpi_real,1,99,MPI_COMM_WORLD,MPI_STATUS_IGNORE,ierr)！status一般定义为MPI_STATUS_IGNORE
!         print*,sum,sum2,myid
!     end if
!     call MPI_FINALIZE (ierr)  
! end Program main

!雅克比迭代代码
program main 
    use mpi 
    implicit none 
    integer,parameter :: steps = 10 
    integer,parameter :: totalsize = 16
    integer,parameter :: mysize = 4 
    integer :: n,myid,numprocs,i,j,rc 
    integer :: left,right,tag1,tag2 
    real :: A(totalsize,mysize+2),B(totalsize,mysize+2) 
    integer :: begin_col,end_col,ierr 
    integer :: status(MPI_STATUS_SIZE) 
    call MPI_INIT(ierr) 
    call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr) 
    call MPI_COMM_SIZE(MPI_COMM_WORLD,numprocs,ierr) 
    print *, "Process ", myid,"of ",numprocs,"is alive!" 
    do j=1,mysize+2 
        do i=1,totalsize 
            A(i,j)=0.0 
        end do 
    end do 
    if(myid==0) then 
        do i=1,totalsize 
            A(i,2)=8.0 
        end do 
    end if 
    if(myid==3) then 
        do i=1,totalsize 
            A(i,mysize+1)=8.0 
        end do 
    end if 
    do i=1,mysize+2 
        A(1,i)=8.0 
        A(totalsize,i)=8.0 
    end do 
    if(myid > 0) then 
        left=myid-1 
    else 
        left=MPI_PROC_NULL 
    end if 
    if(myid < 3) then 
        right=myid+1 
    else 
        right=MPI_PROC_NULL 
    end if 
    tag1=3 
    tag2=4 
    
    do n=1,steps 
        call MPI_SENDRECV(A(1,mysize+1),totalsize,MPI_REAL,right,tag1,& 
                    A(1,1),totalsize,MPI_REAL,left,tag1,MPI_COMM_WORLD,status,ierr) 
        call MPI_SENDRECV(A(1,2),totalsize,MPI_REAL,left,tag2,& 
                    A(1,mysize+2),totalsize,MPI_REAL,right,tag2,MPI_COMM_WORLD,status,ierr) 
        begin_col=2 
        end_col=mysize+1 
        if(myid==0) then 
            begin_col=3 
        end if 
        if(myid==3) then 
            end_col=mysize 
        end if 
        do j=begin_col,end_col 
            do i=2,totalsize-1 
                B(i,j)=(A(i,j+1)+A(i,j-1)+A(i+1,j)+A(i-1,j))*0.25 
            end do 
        end do 
        do j=begin_col,end_col 
            do i=2,totalsize-1 
                A(i,j)=B(i,j) 
            end do 
        end do 
    end do 
    do i=1,totalsize
        print *, myid,(a(i,j),j=begin_col,end_col) 
    end do 
    call MPI_FINALIZE(rc) 
end program


!小练习，主要用于了解语法，算法有瑕疵。(对等模式,虚拟进程，捆绑发送接收mpi_sendr虚拟进程，捆绑发送接收mpi_sendrecvecv)
! program main
!     use mpi
!     implicit none捆绑发送接收mpi_sendrecv)
! program main
!     use mpi
!     implicit none绑发送接收mpi_sendrecv)
! program main
!     use mpi
!     implicit none捆绑发送接收mpi_sendrecv)
! program main
!     use mpi
!     implicit none
!     integer,parameter:: steps=10,totalsize=16,mysize=4!mysize 均分数
!     integer::n,myid,num,i,j
!     integer::left,right,tag1,tag2
!     real(4)::A(totalsize,mysize+2),B(totalsize,mysize+2)!数组声明时候要用常量，用变量会报错。
!     integer::begin,end,ierr,rc
!     integer::status(MPI_STATUS_SIZE)!注意新知识
!     !在Fortran中，status的变量类型为长度是MPI_STATUS_SIZE的整形数组。
!     !通过status(MPI_SOURCE)，status(MPI_TAG)和status(MPI_ERROR)来调用。
!     ! 这三个信息分别返回的值是所收到数据发送源的进程号，该消息的tag值和接收操作的错误代码。
!     call MPI_INIT(ierr)
!     call MPI_COMM_RANK(MPI_COMM_WORLD,myid,ierr)
!     call MPI_COMM_SIZE(MPI_COMM_WORLD,num,ierr)
!     print*,"myid=",myid,"num=",num
!     A=0
!     if(myid==0) then
!         do i=1,totalsize
!             A(i,2)=8.0
!         end do
!     end if
!     if(myid==3) then
!         do i=1,totalsize
!             A(i,5)=8.0
!         end do
!     end if
  
!     do j=1,mysize+2
!         A(1,j)=8.0
!         A(totalsize,j)=8.0
!     end do
!     B=A
!     if(myid>0)then
!         left=myid-1
!     else
!         left=MPI_PROC_NULL
!     end if
    
!     if(myid<3)then
!         right=myid+1
!     else
!         right=MPI_PROC_NULL
!     end if

!     tag1=30
!     tag2=40
!     do n=1,steps
!         call MPI_SENDRECV(A(1,2),totalsize,mpi_real,left,tag1,&
!         A(1,mysize+1),totalsize,mpi_real,right,tag1,MPI_COMM_WORLD,status,ierr)
!         call MPI_SENDRECV(A(1,mysize+1),totalsize,mpi_real,right,tag2,&
!         A(1,1),totalsize,mpi_real,left,tag2,MPI_COMM_WORLD,status,ierr)
!         begin=2
!         end=5
!         if(myid==0) then
!             begin=3
!         end if
!         if(myid==3) then
!             end=4
!         end if

!         do j=begin,end
!             do i=2,totalsize-1
!                 B(i,j)=0.5*(A(i+1,j)+A(i-1,j)+A(i,j+1)+A(i,j-1))
!             end do
!         end do
!         A=B
!     end do
    

!     do i=1,totalsize
!             print*,myid,(A(i,j),j=begin,end)
!     end do
!     call MPI_FINALIZE(rc)

! end program main


!主从结构，矩阵相乘练习
! program name
!     implicit none
!     integer:: i,j




! end program name
! 一个进程可以接收多个进程发送的消息，接收进程并不知道其他进程执行发送消息的顺序。MPI提供了一个特殊的常量MPI_ANY_SOURCE，就可以传递给MPI_Recv。
! status.MPI_SOURCE
! status.MPI_TAG可以通过检查以下两个成员来确定发送者和标签。


