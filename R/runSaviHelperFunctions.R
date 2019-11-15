#' Put passed parameters into one list
#'
#' This is a helper function that is used in runSavi which is not
#' meant for the user.
#'
#' @param coolingScheduleType The type of cooling schedule used
#'     which can be customized by using the parameter
#'     "coolingScheduleParams."  There are currently 4 options.
#'     The argument "Greedy" implements an algorithm which only
#'     moves to a lower (or possibly equal) score.  The argument
#'     "Log" implements a schedule that cools based on a logarithmic
#'     function.  The argument "Step" implements a schedule that
#'     follows a step function.  Finally, the arguement "Adaptive"
#'     will implement a cooling schedule that automatically
#'     determines when to lower the temperature based on the
#'     previous scores.
#' @param coolingScheduleParams A vector of parameters that should
#'     be based on the coolingScheduleType chosen.  For "Greedy,"
#'     pass the number of iterations to use.  For "Log," pass
#'     the number of iterations to use, the alpha constant, and
#'     the beta constant.  Note both alpha and beta must be greater
#'     than 0.  For "Step," pass  the total number of iterations to
#'     run, the initial temperature, the number of iterations to run
#'     before moving down a step, and the percent to step down with.
#'     Note, the percent to step down with must be in the interval
#'     (0,1).  For "Adaptive," pass the total number of iterations
#'     to run, the initial temperature, the number of iterations to
#'     run before checking to lower the temperature, the percent to
#'     step down with, and the threshold of the difference in scores
#'     for the temperature to lower.  Note that the number of iterations
#'     to run before checking to lower the temperature must be an
#'     even  integer. Further, the percent to step down with must be
#'     in the interval (0,1).
#' @return A list of all parameters
#' @examples
#'     extractParams("Greedy",10000)
#'     extractParams("Log",c(10000,1,1))
#'     extractParams("Step",c(10000,20,1000,0.8))
#'     extractParams("Adaptive",c(10000,100,100,0.8,0))
extractParams<-function(coolingScheduleType,coolingScheduleParams){
  params<-list()
  params[["count"]]<-0
  params[["acceptedProps"]]<-0
  params[["type"]]<-coolingScheduleType
  params[["tempArr"]]<-c()
  params[["currTemp"]]<-0
  params[["scoreArr"]]<-c()

  params[["iterations"]]<-coolingScheduleParams[1]

  if(coolingScheduleType=="Log"){
    params[["alpha"]]<-coolingScheduleParams[2]
    params[["beta"]]<-coolingScheduleParams[3]
    params[["currTemp"]]<-params[["alpha"]]/(1+params[["beta"]])

  }else if(coolingScheduleType=="Step"){
    params[["initialTemp"]]<-coolingScheduleParams[2]
    params[["intervalLength"]]<-coolingScheduleParams[3]
    params[["stepDownRate"]]<-coolingScheduleParams[4]
    params[["currTemp"]]<-params[["initialTemp"]]

  }else if(coolingScheduleType=="Adaptive"){
    params[["initialTemp"]]<-coolingScheduleParams[2]
    params[["intervalCheckLength"]]<-coolingScheduleParams[3]

    if(params[["intervalCheckLength"]]%%2==1 | params[["intervalCheckLength"]]<=0){
      stop('The third parameter of the Adaptive cooling schedule parameter array must be a positive even number.')
    }

    params[["stepDownRate"]]<-coolingScheduleParams[4]
    params[["flucThreshold"]]<-coolingScheduleParams[5]
    params[["currTemp"]]<-params[["initialTemp"]]
    params[["countOnCurrTemp"]]<-0
  }

  return(params)
}

#' Get a random proposed state of a given igraph.  To do this, the
#' function switches 2 vertices in different groups and returns an
#' igraph in the new state.
#'
#' This is a helper function that is used in runSavi which is not
#' meant for the user.
#'
#' @param g The current igraph in its current state.
#' @param numOfVertexGroups The desired number of vertex groups for
#'     the graph.  It is required that this number be greater than 1
#'     or the function runSavi will throw an error.  It is recommended
#'     that the number be less than the number of verticies in the
#'     graph.
#' @return A new igraph in the proposed state.
#' @examples
#'     getPropGraph(g,4)
getPropGraphState<-function(g,numOfVertexGroups){
  #get a random non-empty group
  randomGroup<-sampleOne(1:numOfVertexGroups)
  while(length(which(V(g)$group==randomGroup))==0){
    randomGroup<-sampleOne(1:numOfVertexGroups)
  }

  #get a vertex from it
  randomVertex<-sampleOne(which(V(g)$group==randomGroup))

  #Put it in a new group (that may be empty)
  V(g)$group[randomVertex]<-sampleOne(c(1:numOfVertexGroups)[-randomGroup])

  return(g)
}

#' Decide to move to the proposed graph state based on the current
#' and proposed graph states' scores.
#'
#' This is a helper function that is used in runSavi which is not
#' meant for the user.
#
#' @param propScore The score of the proposed graph state.
#' @param currScore The score of the current graph state.
#' @param params A list of parameters based on the cooling schedule.
#' @return boolean
#' @examples
#'     moveToPropGraphAnyway(10,12,params)
moveToPropGraphStateAnyway<-function(propScore,currScore,params){
  if(params$type=="Log"){
    TEM<-params$alpha/(log(params$count+params$beta))
    return((exp((currScore-propScore)/TEM)) > runif(1))

  }else if(params$type=="Step" | params$type=="Adaptive"){
    return((exp((currScore-propScore)/params$currTemp)) > runif(1))
  }
  #If Greedy, always return false
  return(FALSE)
}

#' Update other parameters based on cooling schedule.
#'
#' This is a helper function that is used in runSavi.  It is not
#' meant for the user.
#'
#' @param params The list of parameters
#' @param i The number of iterations of the loop
#'
#' @return params
#' @examples
#'     updateParameters(params,1,"Step")
updateParameters<-function(params,i){
  params$count<-i

  if(params$type=="Log"){
    params$currTemp<-params$alpha/(log(params$count+params$beta))
    params$tempArr<-c(params$tempArr,params$currTemp)
  }
  else if(params$type=="Step"){
    currCountOnInterval<-(params$count+1)%%params$intervalLength
    params$tempArr<-c(params$tempArr,params$currTemp)
    if(currCountOnInterval==0){
      params$currTemp<-params$stepDownRate*params$currTemp
    }
  }
  else if(params$type=="Adaptive"){
    params$countOnCurrTemp<-params$countOnCurrTemp+1
    params$tempArr<-c(params$tempArr,params$currTemp)

    icl<-params$intervalCheckLength

    if(params$countOnCurrTemp>=icl){
      #We get the previous scores on the interval split them into
      #  two parts.

      #Calculate the The first half of the previous interval's scores
      firstHalfScoresAvg<-sum(params$scoreArr[(i-icl+1):(i-icl/2)])/(icl/2)

      #Calculate the The second half of the previous interval's scores
      secondHalfScoresAvg<-sum(params$scoreArr[(i-(icl/2)+1):i])/(icl/2)

      #Determine if we lower the temperature based on the user
      # defined parameter flucThreshold.
      if(secondHalfScoresAvg-firstHalfScoresAvg<params$flucThreshold){
        params$currTemp<-params$stepDownRate*params$currTemp
        params$countOnCurrTemp<-0
      }
    }
  }
  else{
    params$tempArr<-c(params$tempArr,params$currTemp)
  }

  return(params)
}

#' Compare the scores of the current and proposed graph state.
#'
#' This is a helper function that is used in runSavi which is not
#' meant for the user.
#'
#' @param propScore The score of the proposed graph state
#' @param currScore The score of the current graph state
#' @param useStrict A boolean.  If true, then the scores are
#' compared using strict less than.  If false, the scores are
#' compared using less than or equal to.  Note that the default
#' value passed by runSavi is FALSE.
#' @return A the booelan result of the compared scores.
#' @examples
#'     compareScore(8,8,TRUE)
#'     compareScore(6,14,FALSE)
compareScore<-function(propScore, currScore, useStrict){
  if(useStrict){
    return(propScore<currScore)
  }
  return(propScore<=currScore)
}
