#> item.params.convert Centralized Partial Credit Model parameters item parameters calculated from  Partial Credit Model parameters
#'
pcm.to.pcm.cent<-function(item.params) {
  beta<-apply(item.params,1,mean,na.rm=T)
  beta<-cbind(beta,item.params-beta)
  colnames(beta)<-c("beta",paste0("beta.",1:ncol(item.params)))
  beta
}
#> item.params.convert Andersen parameters item parameters calculated from Centralized Partial Credit Model parameters
#'
pcm.to.andersen<-function(item.params) {
  # sum(tau)i=delta
}
#> item.params.convert Power Series Distribution parameters item parameters calculated from Centralized Partial Credit Model parameters
#'

pcm.to.psd<-function(item.params) {

}
#> item.params.convert Conquest parameters item parameters calculated from Centralized Partial Credit Model parameters
#'

pcm.to.conquest<-function(item.params) {

}
#> item.params.convert Partial Credit Model parameters item parameters calculated from Centralized Partial Credit Model parameters
#'
pcm.cent.to.pcm<-function(item.params) {
  item.params[,2:ncol(item.params)]<-item.params[,2:ncol(item.params)]+item.params[,1]
  item.params[,-1]
}
#> item.params.convert Andersen item parameters calculated from Centralized Partial Credit Model parameters
#'
pcm.cent.to.andersen<-function(item.params) {

}
#> item.params.convert Power Series Distribution item parameters calculated from Centralized Partial Credit Model parameters
#'
pcm.cent.to.psd<-function(item.params) {

}
#> item.params.convert Conquest item parameters calculated from Centralized Partial Credit Model parameters
#'
pcm.cent.to.conquest<-function(item.params) {

}
#> item.params.convert Partial Credit Model item parameters calculated from Andersen parameters
#'
andersen.to.pcm<-function(item.params) {
  # Andersen params are easiness params, we make them difficulty params
  ncat<-ncol(item.params)
  pcm.params<-if(ncat==1) -as.data.frame(item.params) else -cbind(item.params[,1],sapply(2:ncat,function(x) item.params[,x]-item.params[,x-1]))
  colnames(pcm.params)<-paste0("tau.",1:ncol(item.params))
  pcm.params
}
#> item.params.convert Centralized Partial Credit Model item parameters calculated from Andersen parameters
#'
andersen.to.pcm.cent<-function(item.params) {

}
#> item.params.convert Power Series Model item parameters calculated from Andersen parameters
#'
andersen.to.psd<-function(item.params) {
  colnames(item.params)<-paste0("gamma.",1:ncol(item.params))
  cbind(data.frame(gamma.0=rep(1,nrow(item.params))),exp(item.params))
}
#> item.params.convert Conquest item parameters calculated from Andersen parameters
#'
andersen.to.conquest<-function(item.params) {
  # betax=psi+psix => betax=psix+sumx(psix)
  # Reverse of: item.params<-sapply(2:ncol(item.params),function(x) {(x-1)*item.params[,1]+ifelse(is.na(item.params[,x-1]),NA,apply(as.matrix(item.params[,2:x]),1,sum,na.rm=T))})

  ncat<-apply(item.params,1,function(x) sum(!is.na(x)))
  nitem<-nrow(item.params)
  if(ncat>1) {
    beta<-sapply(1:nitem,function(x) item.params[x,ncat[x]]/ncat[x])
    taus<-sapply(1:ncol(item.params),function(x) item.params[,x]-beta-(if(x>1) item.params[,x-1] else 0))
      #sapply(ncol(item.params):2,function(x) item.params[,x]<-item.params[,x]-item.params[,x-1])
    # Notice: Andersen parametres are easiness parameters
    item.params<--cbind(beta,taus)

    taucols<-paste0("tau.",1:(ncol(item.params)-1))
  } else taucols<-c()
  colnames(item.params)<-c("beta",taucols)
  item.params

}
#> item.params.convert Partial Credit Model item parameters calculated from Power Series Distribution
#'
psd.to.pcm<-function(item.params) {

}
#> item.params.convert Centralized Partial Credit Model item parameters calculated from Power Series Distribution
#'
psd.to.pcm.cent<-function(item.params) {

}
#> item.params.convert Andersen item parameters calculated from Power Series Distribution
psd.to.andersen<-function(item.params) {
  colnames(item.params)<-paste0("delta.",0:(ncol(item.params)-1))
  log(item.params[,2:ncol(item.params)])
}
#> item.params.convert Conquest item parameters calculated from Power Series Distribution
#'
psd.to.conquest<-function(item.params) {
  # Remove the 1's
  item.params<-item.params[,-1]
  # Get the conquest's
  t(apply(item.params,1,delta.to.conquest.item))
}

#> item.params.convert Partial Credit Model item parameters calculated from Conquest parameters
#'
conquest.to.pcm<-function(item.params=NULL) {
  if(ncol(item.params)>1) {
    item.params[,2][is.na(item.params[,2])]<-0
    item.params<-item.params[,2:ncol(item.params)]+item.params[,1]
  }
  colnames(item.params)<-paste0("tau.",1:ncol(item.params))
  item.params
}
#> item.params.convert Centralized Partial Credit Model item parameters calculated from Conquest parameters
#'

conquest.to.pcm.cent<-function(item.params=NULL) {
  pcm.to.pcm.cent(conquest.to.pcm(item.params))
}
#> item.params.convert Andersen item parameters calculated from Conquest parameters
#'
conquest.to.andersen<-function(item.params=NULL) {
  # betax=psi+psix => betax=psix+sumx(psix)
  #
  if(ncol(item.params)>1)
    item.params<-sapply(2:ncol(item.params),function(x) {(x-1)*item.params[,1]+ifelse(is.na(item.params[,x-1]),NA,apply(as.matrix(item.params[,2:x]),1,sum,na.rm=T))})
  colnames(item.params)<-paste0("cat.",1:ncol(item.params))
  # Andersen parameters are easiness parameters
  -item.params

  #item.params[,1]+item.params[,2:ncol(item.params),]
}
#> item.params.convert Power Series Distribution item parameters calculated from Conquest parameters
#'
conquest.to.psd<-function(item.params=NULL) {
  # ,standardize=c("none","items","cases")
  # standardize<-match.arg(standardize)
  # print(standardize)

  # Returns a matrix of deltas for all items
  # Set the beta of score 0 to 0
  # item.params<-cbind(data.frame(conquest.0=rep(0,nrow(item.params))),item.params)
  # Get the deltas
  #unstandardized
  item.params<-t(apply(item.params,1,conquest.to.delta.item))
  colnames(item.params)<-paste0("gamma.",1:ncol(item.params))
  # switch(standardize,
  #         "items"={
  #           #In case of no LD and DIF, standidardize by setting sum of thresholds to 0
  #           sumofall<-sum(unstandardized)
  #           correction<-sumofall/sum(!apply(unstandardized,1:2,is.na))
  #           standardized<-unstandardized-correction},
  #         "cases"=standardized<-unstandardized, # No idea how to do this correctly
  #         "none"=standardized<-unstandardized
  #        )
  cbind(
    data.frame(gamma.0=rep(1,nrow(item.params))),
    item.params
    )
}

conquest.to.delta.item<-function(single.item.params) {
  #sigma<--sapply(1:length(single.item.params),function(i) sum(single.item.params[1:i]))
  # Returns deltas for this item
  #sapply(sigma,exp)
  sigma.to.delta(conquest.to.sigma.item(single.item.params))
}
conquest.to.sigma.item<-function(single.item.params) {
  single.item.params<-single.item.params[!is.na(single.item.params)]
  mean.param<-mean(as.numeric(single.item.params))
  sapply(1:length(single.item.params),function(i) mean.param+single.item.params[i])
}
sigma.to.delta<-function(sigma) {
# Returns deltas for this item
  sapply(sigma,exp)
}
delta.to.conquest.item<-function(single.item.params) {
  sigma<-sapply(single.item.params,log)
  -sapply(1:length(sigma),function(i) sigma[i]-ifelse(i==1,0,sigma[i-1]))
}
#andersen.to.conquest.item<-function(single.item.params) {
#   item.levels<-length(single.item.params)
#   item.param<-1/item.levels*sum(single.item.params)
#   single.item.params-item.param
# }

#' Convert item parameters from one parametrization to another
#'
#' @param from.model A model of class TAM or eRm.
#' @param item.params A matrix of item parameters (items in rows, thresholds in columns) (not needed if from.model is given)
#' @param from,to Type of item parameters. One of pcm, pcm.cent, andersen, psd, conquest (from not needed if from.model is given).
#' @param return.vector Get result as a vector of category values instead of the default data.frame
#' @return Returns item parameters in the parametrization specified in *to*.
#' @export
#' @details
#' The Rasch model can be parameterized in multiple ways. This function translates parameters from one to the other. The following models are supported:
#' \itemize{
#'   \item{Power series distribution parametrization (psd)}{\cr
#'     \eqn{\xi^x\gamma_x \over G(\xi,\gamma_1 ... \gamma_k)}, where \eqn{\xi} is the person parameter (ability), and \eqn{\gamma_x} are the item parameters. Related through \eqn{\xi=exp(\theta)} and \eqn{\gamma_x=exp(\delta_x)} to the Andersen parametrization
#'   }
#'   \item{Andersen parametrization (andersen)} {\cr
#'     \eqn{exp(x\theta+\delta_x) \over G(\theta,\delta_1 ... \delta_k)}, where \eqn{\theta} is the person parameter (ability), and \eqn{\delta_x} are the item (easiness) parameters
#'   }
#'   \item{Partial Credit Model (PCM)/Masters' parametrization (pcm)} {\cr
#'     \eqn{exp(x\theta-\sum\limits_{i=1}^x\tau_i) \over G(\theta,\tau_1 ... \tau_k)}, where \eqn{\theta} is the person parameter (ability), and \eqn{\tau_x} are the item step parameters
#'   }
#'   \item{Partial Credit Model (PCM)/Masters' parametrization with centralized item step parameters (pcm.cent)} {\cr
#'     \eqn{exp(x(\theta-\beta)-\sum\limits_{i=1}^x\beta_i) \over G(\theta,\beta_1 ... \beta_k)}, where \eqn{\theta} is the person parameter (ability), \eqn{\beta={1 \over k}\sum\limits_{i=1}^k \tau_i} is the average of the \eqn{\tau} parameters from the PCM parametrization, and \eqn{\beta_x=\tau_x-{1 \over k}\sum\limits_{i=1}^k \tau_i} are the centralized item step parameters
#'   }
#'   \item{Conquest parametrization (conquest)} {\cr
#'     \eqn{exp(x(\theta-\psi-\sum\limits_{i=1} x\psi_x) \over G(\theta,\psi_1 ... \psi_k)}, where \eqn{\sum_limits{i=1}^x\psi_i \equiv 0}. And where \eqn{\theta} is the person parameter (ability), \eqn{\psi={1 \over k}\sum\limits_{i=1}^k \tau_i} is the average of the \eqn{\tau} parameters from the PCM parametrization, and \eqn{\psi_x=\tau_x-{1 \over k}\sum\limits_{i=1}^k \tau_i} are the centralized item step parameters
#'   }
#' }
#' [TAM] uses Conquest parametrization. [eRm] uses Andersen parametrization. RUMM 2030 uses Partial Credit parametrization with centralized item step parameters. DIGRAM uses Power Series Distribution parametrization.
#' @author Jeppe Bundsgaard & Svend Kreiner
#' @references
#' Andersen, B. E. (1970). Asymptotic properties of conditional likelihood estimators. *Journal of the Royal Statistical Society*, Series B, 32,283-301.
#'
#' Brown, N. J. S. (2004). Interpreting Ordered Partition Model Parameters from ConQuest. [https://bearcenter.berkeley.edu/sites/default/files/report%20-%20opm_parameters.pdf]
#'
#' Hatzinger, R., & Rusch, T. (2009). IRT models with relaxed assumptions in eRm: A manual-like instruction. *Psychology Science Quarterly*, 51(1), 87–120.
#'
#' Kreiner, S. (n.d.). Om beregning af item-parametre I TAM.
#'
#' Kreiner, s. (n.d.). Parameterization of graphical loglinear Rasch models.
#'
#' Bundsgaard, J. &  Kreiner, S. (2019). *Undersøgelse af De Nationale Tests måleegenskaber*. 2nd Ed. Copenhagen: DPU, Aarhus University.
#'
#' Masters, G. N. (1982). A Rasch model for partial credit scoring. *Psychometrika*, 47(2), 149–174.

#' @examples
#' item.params<-matrix(c(0,1,2,3,1,2,3,4),nrow=4)
#' item.params.convert(item.params=item.params,from="conquest",to="psd")
item.params.convert<-function(from.model=NULL,item.params=c(),from=c("pcm","pcm.cent","andersen","psd","conquest"),to=c("pcm","pcm.cent","andersen","psd","conquest"), return.vector=F) {
  if(is.null(from.model)) {
    if(!is.null(attributes(item.params)$par.type)) {
      from<-attributes(item.params)$par.type
    } else from<-match.arg(from)
  } else {
    from<-switch(intersect(class(from.model),c("tam.mml","eRm")),"tam.mml"="conquest","eRm"="andersen")
    item.params<-get.item.params(from.model,type=from)
  }
  if(inherits(item.params,"numeric")) {item.params<-matrix(item.params,nrow=1)}
  if(!inherits(item.params,"matrix")) {item.params<-as.matrix(item.params)}
  to<-match.arg(to)
  if(from==to) {return(item.params)}
  item.params<-switch(from,
         "conquest"=switch(to,
                       "psd"=andersen.to.psd(conquest.to.andersen(item.params = item.params)),
                       "andersen"=conquest.to.andersen(item.params = item.params), #Okay!
                       "pcm"=conquest.to.pcm(item.params = item.params),
                       "pcm.cent"=conquest.to.pcm.cent(item.params = item.params),
                       stop("This conversion is not implemented yet, sorry!")
         ),
         "psd"=switch(to,
                      "conquest"=andersen.to.conquest(psd.to.andersen(item.params = item.params)),
                      "andersen"=psd.to.andersen(item.params = item.params),
                      "pcm"=andersen.to.pcm(psd.to.andersen(item.params = item.params)),
                      "pcm.cent"=pcm.to.pcm.cent(andersen.to.pcm(psd.to.andersen(item.params = item.params))),
                      stop("This conversion is not implemented yet, sorry!")
         ),
         "andersen"=switch(to,
                           "psd"=andersen.to.psd(item.params = item.params),
                           "pcm"=andersen.to.pcm(item.params = item.params),
                           "pcm.cent"=pcm.to.pcm.cent(andersen.to.pcm(item.params = item.params)),
                           "conquest"=andersen.to.conquest(item.params = item.params), # Okay!
                           stop("This conversion is not implemented yet, sorry!")
         ),
         "pcm"=switch(to,
                      #"psd"=pcm.to.psd(item.params = item.params),
                      "pcm.cent"=pcm.to.pcm.cent(item.params = item.params),
                      #"andersen"=pcm.to.andersen(item.params = item.params),
                      stop("This conversion is not implemented yet, sorry!")
         ),
         "pcm.cent"=switch(to,
                      #"psd"=pcm.cent.to.psd(item.params = item.params),
                      "pcm"=pcm.cent.to.pcm(item.params = item.params),
                      #"andersen"=pcm.cent.to.andersen(item.params = item.params),
                      stop("This conversion is not implemented yet, sorry!")
         ),
         stop("This conversion is not implemented yet, sorry!")
  )
  attributes(item.params)$par.type<-to
  attributes(item.params)$from.type<-from

  if(return.vector && is.matrix(item.params))
    item.params<-unlist(apply(item.params, 1, function(x) x[!is.na(x)]))
  item.params
}

