#' Iterative Proportional Fitting (ipf)
#'
#' @param X A matrix, e.g. of persons responses (rows) to items (columns)
#' @param Xhat Initial values of the solution (optional). Defaults to 0.5 in all cells
#' @param Delta Interaction parameters. A matrix or scalar. Defaults to 1.
#' @param func A function of parameters i, j (row and column numbers), A, B (current estimations),  and Delta.
#' @param anchor A function to anchor A or B estimates. Defaults to NULL.
#' @param v Verbose level (0: none, 1: progression, 2: verbose)
#'
#' @return Returns a list of estimates Ahat, Bhat, and Xhat
#' @details This function does the estimation. Used by specific estimation functions.
#' @export
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references Svend Kreiner (manuscript, 2023). *Appendix H - Estimation of Rasch parameters by iterative proportional fitting*.
#' @examples
#' Example 1. 20 (12 with none-extreme scores) persons and four items
#' X<-matrix(data=c(0,0,0,1,1,1,0,1,0,0,0,1,
#'                  1,1,1,0,0,0,0,0,1,0,1,1,
#'                  1,1,0,0,0,1,1,0,1,1,0,1,
#'                  1,0,0,0,0,0,0,0,0,0,0,0),ncol=4)
#' ipf(
#'   x=X,
#'   func=function(i,j,Xsi,Delta,InteractionDelta) { 1/(1+Xsi[i]*Delta[j]) },
#'   anchor=function(Xsi,Delta) { prod(Delta,na.rm = T)^(1/length(Delta)) },
#'   v=2
#' )
#'
#' # Example 2. 517 persons in 6 score groups and 7 items
#' X<-matrix(data=c(55,78,113,86,58,32,
#'                  1,10,16,31,33,27,
#'                  17,60,98,80,53,31,
#'                  6,9,20,27,26,28,
#'                  16,42,77,75,52,32,
#'                  1,5,9,20,20,18,
#'                  3,16,42,49,48,30),ncol=7)
#'
#' nr=rowSums(X)/(1:nrow(X))
#'
#' est<-ipf(
#'   X=X,
#'   func=function(i,j,Xsi,Delta,InteractionDelta) { nr[i]*1/(1+Xsi[i]*Delta[j]) },
#'   anchor=function(Xsi,Delta) { prod(Delta,na.rm = T)^(1/length(Delta)) },
#'   v=2
#' )
#'
#' # Estimate of conditional probability of positive response:
#' Xhat<-est$Xhat
#' Xhat/nr

# Conditional estimates of item parameters
# nr=rowSums(X)/(1:nrow(X))
# ipf(
#   X=X,
#   #Bhat=colSums(X)/sum(X),
#   Rhat=rowSums(X),
#   Bhat=(colSums(X)/sum(X))/(prod(colSums(X)/sum(X))^(1/ncol(X))), # Initial value: Relative frequencies of item margins standardized to product equals 1)
#   recalcRows=F,
#   func=function(i,j,Xsi,Delta,InteractionDelta) {
#     res<-Delta[j]*nr[i]*gamma_matrix(item.params = matrix(c(rep(1,length(Delta)-1),Delta[-j]),ncol=2),R = i-1)/gamma_matrix(item.params = matrix(c(rep(1,length(Delta)),Delta),ncol=2),R = i)
#     # print(res)
#     res
#   },
#   anchor=function(Xsi,Delta) { prod(Delta,na.rm = T)^(1/length(Delta)) },
#   v=2
# )


ipf<-function(X=matrix(),
              Xhat=matrix(0.5,nrow = nrow(X),ncol = ncol(X)),Ahat=NULL,Bhat=NULL,Rhat=NULL,Chat=NULL,
              Delta=1,func=function(i,j,A,B,Delta) {1},anchor=NULL,
              recalcRows=T,recalcCols=T,  smallvalue=.0001,v=1) {
  # Row- and Column sums
  V<-R<-rowSums(X,na.rm = T) # na.rm means that structural zeros are considered 0
  W<-C<-colSums(X,na.rm = T) # do.
  nR<-nrow(X)
  nC<-ncol(X)
  # Initial parameters
  if(is.null(Ahat)) Ahat=rep(1,nR)
  if(is.null(Bhat)) Bhat=rep(1,nC)
  if(is.null(Rhat)) Rhat=rep(NA,nR)
  if(is.null(Chat)) Chat=rep(NA,nC)
  found<-F
  iteration<-0
  jump<-0
  while (!found) {
    if(v>0) jump<-ipf.verbose(v,iteration,Xhat,Ahat,Bhat,R,C,jump)
    if(jump==0) break
    iteration<-iteration+1

    # Calculations
      for(i in 1:nR) {
        delta<-if(is.matrix(Delta)) Delta[i] else Delta
        Rhat[i]<-Ahat[i]*sum(Bhat*delta*sapply(1:nC,function(x) func(i,x,Ahat,Bhat,Delta)))
      }
      for(j in 1:nC) {
        delta<-if(is.matrix(Delta)) Delta[j] else Delta
        Chat[j]<-Bhat[j]*sum(Ahat*delta*sapply(1:nR,function(x) func(x,j,Ahat,Bhat,Delta)))
      }
    if(max(abs(V-Rhat))<smallvalue && max(abs(W-Chat))<smallvalue) {
      found<-T
    } else {
      # Proportional adjustment
      Ahat<-Ahat*V/Rhat
      Bhat<-Bhat*W/Chat
      # Anchoring
      if(!is.null(anchor)) {
        anchorfactor<-anchor(Ahat,Bhat)
        Ahat<-Ahat*anchorfactor
        Bhat<-Bhat/anchorfactor
      }
    }
    # Producing Xhat
    if(found || (v>1 & jump<=iteration)) {
      for(i in 1:nR) {
        for(j in 1:nC) {
          delta<-if(is.matrix(Delta)) Delta[i,j] else Delta
          Xhat[i,j]<-Ahat[i]*Bhat[j]*delta*func(i,j,Ahat,Bhat,Delta)
        }
      }
      if(found) ipf.verbose(v,iteration,Xhat,Ahat,Bhat,R,C,-1)
      #Ri<-rowSums(Xhat,na.rm = T)
      #Ci<-colSums(Xhat,na.rm = T)
    }

  }
  list(Ahat=Ahat,Bhat=Bhat,Xhat=Xhat)
}

ipf.verbose<-function(v,iteration,Xhat,Ahat,Bhat,R,C,jump) {
  if(v>0) cat(ifelse(jump==-1 || iteration %% 10==0,iteration,"*"))
  if(v>1 & jump<=iteration) {
    all<-Xhat
    all<-rbind(all,colSums(all))
    all<-cbind(all,rowSums(all))

    all<-rbind(all,c(C,NA))
    all<-cbind(all,c(R,NA,NA))

    all<-rbind(all,c(Bhat,NA,NA))
    all<-cbind(all,c(Ahat,NA,NA,NA))

    cat("\n")
    print(all)
    if(jump!=-1) {
      user_input <- readline("Jump?  ")
      jump<-if(user_input == '0') 0 else iteration+as.integer(user_input)
    }
  }
  jump
}

