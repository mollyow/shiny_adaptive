
standard_error <- function(x) {
  sd(x) / sqrt(length(x))
}


# Same as "apply_floor" in main codebase
impose_floor <- function(a, amin) {
  new <- pmax(a, amin)
  total_slack <- sum(new) - 1
  individual_slack <- new - amin
  c <- total_slack / sum(individual_slack)
  new - c * individual_slack
}


#' Approximates Thompson Sampling probabilities
#' (i.e. probability that each arm is maximal)
bernoulli_thompson_sampling_probabilities <- function(successes, failures, 
                                                      num_mc = 100, floor = 0.01) {
  # Number of arms
  K <- length(successes)
  # 1) Sample model: theta[k] ~ Beta(successes[k], failures[k])
  draws <- replicate(num_mc, rbeta(K, successes + 1, failures + 1)) # + 1 is prior
  # 2) Check out many times each arm was maximal
  argmax <- apply(draws, 2, which.max)
  # 3) Tally up the probailities
  ts_probs <- unname(table(cut(argmax, seq(0, K))) / num_mc)
  # 4) Apply floor, if appropriate
  impose_floor(ts_probs, floor)
}

bernoulli_thompson_sampling_probabilities_b <- function(scores,
                                                        num_mc = 100, 
                                                        floor = 0.01) {
  K <- ncol(scores)
  t <- nrow(scores)
  # 0) predict outcome means and variances for each arm
  muhats <- colMeans(scores)
  
  if(t<=3){
    sigmahats <- c(.5,.5)/sqrt(t)
  } else {
    sigmahats <- apply(scores, 2, function(x) standard_error(x))
  }
  
  # 1) Sample from each of the relevant Normal distributions num_mc times:
  draws <- replicate(num_mc, rnorm(K, mean = muhats, sd = sigmahats))
  # 2) Check how many times wth arm was maximal
  argmax <- apply(draws, 2, which.max)
  # 3) Tally up the probailities
  ts_probs <- unname(table(cut(argmax, seq(0, K))) / num_mc)
  # 4) Apply floor, if appropriate
  return(impose_floor(ts_probs, floor))
}


# Epsilon-greedy probabilities
bernoulli_epsilon_greedy_probabilities <- function(successes, failures, 
                                                   epsilon_rate = 0.5,
                                                   floor = 0) {
  K <- length(successes)
  trials <- successes + failures
  if (any(trials == 0)) {
    return(rep(1 / K, K))
  } else {
    # Current period
    t <- sum(trials)
    # Current epsilon: 1/K * 1/t^{decay rate}
    epsilon <- 1 / K * 1 / t^epsilon_rate
    # Choose best arm with 1 - epsilon probability
    greedy_arm <- which.max(successes / trials)
    eg_probs <- rep(epsilon / K, K)
    eg_probs[greedy_arm] <- epsilon / K + (1 - epsilon)
    eg_probs <- impose_floor(eg_probs, floor)
    return(eg_probs)
  }
}
bernoulli_epsilon_greedy_probabilities_b <- function(scores, 
                                                     epsilon_rate = 0.5,
                                                     floor = 0.01) {
  K <- ncol(scores)
  t <- nrow(scores)
  if (t<K) {
    return(rep(1 / K, K))
  } else {
    # 0) predict outcome means and variances for each arm
    muhats <- colMeans(scores)
    # Current epsilon: 1/K * 1/t^{decay rate}
    epsilon <- 1 / K * 1 / t^epsilon_rate
    # Choose best arm with 1 - epsilon probability
    greedy_arm <- which.max(muhats)
    eg_probs <- rep(epsilon / K, K)
    eg_probs[greedy_arm] <- epsilon / K + (1 - epsilon)
    eg_probs <- impose_floor(eg_probs, floor)
  }
}


bernoulli_ucb_probabilities <- function(successes, 
                                        failures) {
  K <- length(successes)
  trials <- sum(successes + failures)
  ucb_probs <- rep(0, K)
  ucb <- successes/trials + sqrt(1 * log(sum(trials))/trials)
  ucb_arm <-  sample(which(ucb == max(ucb)), 1)
  ucb_probs[ucb_arm] <- 1
  return(ucb_probs)
}



run_bernoulli_experiment <- function(
  means, 
  algorithm, 
  T, 
  initial_batch = 0,
  floor = 0,
  num_batches = 100) {
  
  # Number of arms
  K <- length(means)
  
  # Draw potential outcomes
  ys <- t(replicate(T, rbinom(n = K, size = 1, prob = means)))
  
  # Initialize statistics
  successes <- rep(0, K)
  failures <- rep(0, K)
  
  # Initialize output
  e <- matrix(NA, T, K)
  w <- rep(NA, T)
  yobs <- rep(NA, T)
  regret <- rep(NA, T)
  
  if(num_batches> (T-initial_batch)){
    num_batches <- (T-initial_batch)
  }
  
  adpt_idx <- (initial_batch + 1):T
  update_times <- ceiling(as.numeric(sub("\\((.+),.*", "\\1", 
                                         unique(cut(adpt_idx, num_batches)))))
  
  # Run experiment
  for (t in seq(T)) {
    
    if (t <= max(initial_batch, 1) ) {
      # During initial phase,
      # choose arms at random
      e[t,] <- rep(1 / K, K)
    } else {
      et <- e[1:(t-1),,drop=FALSE]
      wt <- w[1:(t-1)]
      yobst <- yobs[1:(t-1)]
      ipw_scores <- sapply(seq(K), function(k) yobst*(wt==k)/(et[, k]))
      if(is.null(dim(ipw_scores))){
        dim(ipw_scores) <- c(t-1, K) 
      } 
      # During adaptive phase, compute assignment
      # probabilities according to algorithm
      if(t %in% update_times){
        e[t, ] <- switch(algorithm,
                         "thompson_sampling" = bernoulli_thompson_sampling_probabilities(successes, failures, floor = floor),
                         "epsilon_greedy" = bernoulli_epsilon_greedy_probabilities(successes, failures, floor = floor),
                         "ucb" = bernoulli_ucb_probabilities(successes, failures),
                         "random" = rep(1 / K, K),
                         "thompson_sampling_balanced" = 
                           bernoulli_thompson_sampling_probabilities_b(
                             ipw_scores, 
                             floor = floor),
                         "epsilon_greedy_balanced" = 
                           bernoulli_epsilon_greedy_probabilities_b(
                             ipw_scores,
                             floor = floor)
        ) 
      } else {
        e[t, ] <- e[(t-1), ]
      }
    }
    
    # Draw from the thompson sampling probabilities
    w[t] <- sample.int(K, size = 1, prob = e[t, ])
    
    # Observe reward given arm
    yobs[t] <- ys[t, w[t]]
    
    # Update model statistics
    if (yobs[t] == 1) {
      successes[w[t]] <- successes[w[t]] + 1
    } else {
      failures[w[t]] <- failures[w[t]] + 1
    }
    
    # Compute average regret (note this is not possible in real life)
    regret[t] <- max(means) - mean(means[w[1:t]])
  }
  
  # Return experiment results
  list(yobs = yobs, w = w, e = e, regret = regret, 
       means = means, T = T, algorithm = algorithm,
       floor = floor)
}

run_bernoulli_control_experiment <- function(
  means, 
  algorithm, 
  T, 
  initial_batch = 0,
  floor = 0,
  num_batches = 100,
  control = NULL) {
  
  # Number of arms
  K <- length(means)
  if(is.null(control)){ control <- 1/K }
  
  # Draw potential outcomes
  ys <- t(replicate(T, rbinom(n = K, size = 1, prob = means)))
  
  # Initialize statistics
  successes <- rep(0, K)
  failures <- rep(0, K)
  
  # Initialize output
  e <- matrix(NA, T, K)
  w <- rep(NA, T)
  yobs <- rep(NA, T)
  regret <- rep(NA, T)
  
  if(num_batches> (T-initial_batch)){
    num_batches <- (T-initial_batch)
  }
  
  adpt_idx <- (initial_batch + 1):T
  update_times <- ceiling(as.numeric(sub("\\((.+),.*", "\\1", 
                                         unique(cut(adpt_idx, num_batches)))))
  
  # Run experiment
  for (t in seq(T)) {
    
    if (t <= max(initial_batch, 1) ) {
      # During initial phase,
      # choose arms at random
      e[t,] <- rep(1 / K, K)
    } else {
      et <- e[1:(t-1),,drop=FALSE]
      wt <- w[1:(t-1)]
      yobst <- yobs[1:(t-1)]
      ipw_scores <- sapply(seq(K), function(k) yobst*(wt==k)/(et[, k]))
      if(is.null(dim(ipw_scores))){
        dim(ipw_scores) <- c(t-1, K) 
      } 
      # During adaptive phase, compute assignment
      # probabilities according to algorithm
      if(t %in% update_times){
        e[t, ] <- switch(algorithm,
                         "thompson_sampling" = c(control, (1-control)*bernoulli_thompson_sampling_probabilities(successes[-1], failures[-1], floor = floor)),
                         "epsilon_greedy" = c(control, (1-control)*bernoulli_epsilon_greedy_probabilities(successes[-1], failures[-1])),
                         "thompson_sampling_balanced" = 
                           c(control, (1-control)*bernoulli_thompson_sampling_probabilities_b(
                             ipw_scores[,-1,drop=FALSE], 
                             floor = floor)),
                         "epsilon_greedy_balanced" = 
                           c(control, (1-control)*bernoulli_epsilon_greedy_probabilities_b(
                             ipw_scores[,-1,drop=FALSE],
                             epsilon_rate = 0.5,
                             floor = floor)),
                         "random" = rep(1 / K, K)
        ) 
      } else {
        e[t, ] <- e[(t-1), ]
      }
    }
    
    # Draw from the thompson sampling probabilities
    w[t] <- sample.int(K, size = 1, prob = e[t, ])
    
    # Observe reward given arm
    yobs[t] <- ys[t, w[t]]
    
    # Update model statistics
    if (yobs[t] == 1) {
      successes[w[t]] <- successes[w[t]] + 1
    } else {
      failures[w[t]] <- failures[w[t]] + 1
    }
    
    # Compute regret (note this is not possible in real life)
    regret[t] <- max(means) - mean(means[w[1:t]])
  }
  
  # Return experiment results
  list(yobs = yobs, w = w, e = e, regret = regret, 
       means = means, T = T, algorithm = algorithm,
       floor = floor)
}


# statistics ----

sample_mean_statistics <- function(result) {
  K <- ncol(result$e)
  m <- sapply(seq(K), function(k) mean(result$yobs[result$w == k], na.rm = TRUE))
  se <- sapply(seq(K), function(k) standard_error(na.omit(result$yobs[result$w == k])))
  bias <- m - result$means
  tstat <- ifelse(se > 0, m / se, 0)
  df <- data.frame(
    method = "sample_mean",
    arm = as.factor(seq(K)),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage = ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  )
  stats <- melt(df, id.vars = c("arm", "method"))
  stats
}

ipw_statistics <- function(result) {
  K <- ncol(result$e)
  ipw <- sapply(seq(K), function(k) result$yobs * (result$w == k) / result$e[, k])
  m <- apply(ipw, 2, mean)
  se <- apply(ipw, 2, standard_error)
  bias <- m - result$means
  tstat <- ifelse(se > 0, m / se, 0)
  stats <- melt(data.frame(
    method = "ipw",
    arm = as.factor(seq(K)),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage =  ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  ),
  id.vars = c("arm", "method"))
  stats
}

haj_statistics <- function(result) {
  K <- ncol(result$e)
  haj <- sapply(seq(K), function(k) result$yobs * (result$w == k) / result$e[, k] / sum(1/result$T *(result$w == k) / result$e[, k]))
  m <- apply(haj, 2, mean)
  se <- apply(haj, 2, standard_error)
  bias <- m - result$means
  tstat <- ifelse(se > 0, m / se, 0)
  stats <- melt(data.frame(
    method = "haj",
    arm = as.factor(seq(K)),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage =  ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  ),
  id.vars = c("arm", "method"))
  stats
}


# difference statistics ----
# arm 1 - other arms
sample_mean_diff_statistics <- function(result) {
  K <- ncol(result$e)
  est <- sapply(seq(K), function(k) mean(result$yobs[result$w == k], na.rm = TRUE))
  sest <- sapply(seq(K), function(k) standard_error(na.omit(result$yobs[result$w == k])))
  m <- est[1] - est[-1] 
  ns <- sapply(seq(K), function(k) sum(result$w == k, na.rm = TRUE))
  se <- sqrt( sest[1]^2 / ns[1] + sest[-1]^2 / ns[-1] )
  bias <- m - (result$means[1]-result$means[-1])
  tstat <- ifelse(se > 0, m / se, 0)
  df <- data.frame(
    method = "sample_mean",
    arm = as.factor(seq(K)[-1]),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage = ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  )
  stats <- melt(df, id.vars = c("arm", "method"))
  stats
}

ipw_diff_statistics <- function(result) {
  K <- ncol(result$e)
  ipw <- sapply(seq(K), function(k) result$yobs * (result$w == k) / result$e[, k])
  scores <- ipw[,1]- ipw[,-1]
  m <- colMeans(scores)
  se <- apply(scores, 2, standard_error)
  bias <- m - (result$means[1] - result$means[-1])
  tstat <- ifelse(se > 0, m / se, 0)
  stats <- melt(data.frame(
    method = "ipw",
    arm = as.factor(seq(K)[-1]),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage =  ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  ),
  id.vars = c("arm", "method"))
  stats
}

haj_diff_statistics <- function(result) {
  K <- ncol(result$e)
  haj <- sapply(seq(K), function(k) result$yobs * (result$w == k) / result$e[, k] / sum(1/result$T *(result$w == k) / result$e[, k]))
  scores <- haj[,1] - haj[,-1]
  m <- colMeans(scores)
  se <- apply(scores, 2, standard_error)
  bias <- m - (result$means[1] - result$means[-1])
  tstat <- ifelse(se > 0, m / se, 0)
  stats <- melt(data.frame(
    method = "haj",
    arm = as.factor(seq(K)[-1]),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage =  ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  ),
  id.vars = c("arm", "method"))
  stats
}

# control statistics ----
# other arms - arm 1
sample_mean_control_statistics <- function(result) {
  K <- ncol(result$e)
  est <- sapply(seq(K), function(k) mean(result$yobs[result$w == k], na.rm = TRUE))
  sest <- sapply(seq(K), function(k) standard_error(na.omit(result$yobs[result$w == k])))
  m <- est[-1] - est[1]
  ns <- sapply(seq(K), function(k) sum(result$w == k, na.rm = TRUE))
  se <- sqrt( sest[1]^2 / ns[1] + sest[-1]^2 / ns[-1] )
  bias <- m - (result$means[-1]- result$means[1])
  tstat <- ifelse(se > 0, m / se, 0)
  df <- data.frame(
    method = "sample_mean",
    arm = as.factor(seq(K)[-1]),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage = ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  )
  stats <- melt(df, id.vars = c("arm", "method"))
  stats
}

ipw_control_statistics <- function(result) {
  K <- ncol(result$e)
  ipw <- sapply(seq(K), function(k) result$yobs * (result$w == k) / result$e[, k])
  ipw_cum <- apply(ipw[,-1], 2, function(x) cumsum(x) / seq_along(x))
  best <- apply(ipw_cum, 1, which.max)
  scores <- ipw[,-1] - ipw[,1]
  best_scores <- scores[cbind(2:result$T, best[-result$T])]
  m <- c(colMeans(scores), mean(best_scores))
  se <- c(apply(scores, 2, standard_error), standard_error(best_scores))
  bias <- m - c(c(result$means[-1] - result$means[1]), 
                (mean(result$means[-1][best]) -result$means[1]) )
  tstat <- ifelse(se > 0, m / se, 0)
  stats <- melt(data.frame(
    method = "ipw",
    arm = as.factor(c(seq(K)[-1], 'best')),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage =  ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  ),
  id.vars = c("arm", "method"))
  stats
}

haj_control_statistics <- function(result) {
  K <- ncol(result$e)
  haj <- sapply(seq(K), function(k) result$yobs * (result$w == k) / result$e[, k] / sum(1/result$T *(result$w == k) / result$e[, k]))
  haj_cum <- apply(haj[,-1], 2, function(x) cumsum(x) / seq_along(x))
  best <- apply(haj_cum, 1, which.max)
  scores <- haj[,-1] - haj[,1]
  best_scores <- scores[cbind(2:result$T, best[-result$T])]
  m <- c(colMeans(scores), mean(best_scores))
  se <- c(apply(scores, 2, standard_error), standard_error(best_scores))
  bias <- m - c(c(result$means[-1] - result$means[1]), 
                (mean(result$means[-1][best]) -result$means[1]) )
  tstat <- ifelse(se > 0, m / se, 0)
  stats <- melt(data.frame(
    method = "haj",
    arm = as.factor(c(seq(K)[-1], 'best')),
    estimate = m,
    bias = bias,
    stderr = se,
    tstat = tstat,
    coverage =  ifelse(se > 0, as.numeric(abs(bias) / se < 1.96), 0),
    power = as.numeric(abs(tstat) > 1.96)
  ),
  id.vars = c("arm", "method"))
  stats
}




# plotting ----

plot_regret <- function(results) {
  ylims <- c(0, max(abs(diff(range(results[[1]]$means))), .1))
  df <- do.call(rbind, 
                mapply(function(r, i) {
                  data.frame(factor(i), 1:length(r$regret), r$regret) 
                }, results, seq_along(results),
                SIMPLIFY = F))
  colnames(df) <- c("id", "time", "regret")
  df <- melt(df, id.vars=c("id", "time"), variable.name = "regret")
  ggplot(df, aes(x = time, y = value)) +
    coord_cartesian(ylim=ylims) + 
    geom_line(aes(x = time, y = value, group = id), alpha = 0.2) + 
    geom_smooth() + 
    xlab("time") +
    ylab("") +
    ggtitle("Average regret")
}


plot_assignment_probabilities <- function(results, color = FALSE) {
  K <- ncol(results[[1]]$e)
  T <- nrow(results[[1]]$e)
  df <- do.call(rbind, 
                mapply(function(r, i) {
                  data.frame(factor(i), 1:nrow(r$e), r$e) 
                }, results, seq_along(results),
                SIMPLIFY = F))
  colnames(df) <- c("id", "time", seq(K))
  df <- melt(df, id.vars=c("id", "time"), variable.name = "arm")
  if(color){
    ggplot(df, aes(x = time, y = value)) +
      geom_line(aes(group = id, color = id)) + 
      geom_smooth(span = 1/length(unique(df$time))*10) +
      facet_wrap(. ~arm, nrow = ceiling(K/3)) + 
      xlab("time") +
      ylab("") +
      coord_cartesian(ylim=c(0,1)) + 
      ggtitle("Assignment probabilities") +
      theme(legend.position='none')
  } else {
    ggplot(df, aes(x = time, y = value)) +
      geom_line(aes(group = id), alpha=0.2) + 
      geom_smooth() +
      facet_wrap(. ~arm, nrow = ceiling(K/3)) + 
      xlab("time") +
      ylab("") +
      coord_cartesian(ylim=c(0,1)) + 
      ggtitle("Assignment probabilities")
  }
}


plot_inference <- function(results, 
                           statistics = c("estimate", "bias",
                                          "stderr", "tstat",
                                          "coverage", "power"), 
                           estimators = c('sm', 'ipw', 'haj')) {
  sm <- do.call(rbind, Map(sample_mean_statistics, results))
  ipw <- do.call(rbind, Map(ipw_statistics, results))
  haj <- do.call(rbind, Map(haj_statistics, results))
  df <- do.call(rbind, lapply(estimators, dynGet))
  ncol <- ifelse(length(statistics) <= 3, 1, 2)
  p <- ggplot(data = subset(df, variable %in% statistics), 
              aes(x = arm, y = value, fill = method)) +
    stat_summary(geom = "bar", fun = mean, position = "dodge") +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
    facet_wrap(. ~ variable, scales = "free", ncol = ncol) +
    ggtitle("Inference")
}

plot_diff_inference <- function(results, 
                                statistics = c("estimate", "bias",
                                               "stderr", "tstat",
                                               "coverage", "power"),
                                estimators = c('sm', 'ipw', 'haj')) {
  sm <- do.call(rbind, Map(sample_mean_diff_statistics, results))
  ipw <- do.call(rbind, Map(ipw_diff_statistics, results))
  haj <- do.call(rbind, Map(haj_diff_statistics, results))
  df <- do.call(rbind, lapply(estimators, dynGet))
  ncol <- ifelse(length(statistics) <= 3, 1, 2)
  p <- ggplot(data = subset(df, variable %in% statistics), 
              aes(x = arm, y = value, fill = method)) +
    stat_summary(geom = "bar", fun = mean, position = "dodge") +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
    facet_wrap(. ~ variable, scales = "free", ncol = ncol) +
    ggtitle("Inference (arm 1 - arm)")
}

plot_control_inference <- function(results, 
                                   statistics = c("estimate", "bias",
                                                  "stderr", "tstat",
                                                  "coverage", "power"),
                                   estimators = c('ipw', 'haj')) {
  sm <- do.call(rbind, Map(sample_mean_control_statistics, results))
  ipw <- do.call(rbind, Map(ipw_control_statistics, results))
  haj <- do.call(rbind, Map(haj_control_statistics, results))
  df <- do.call(rbind, lapply(estimators, dynGet))
  ncol <- ifelse(length(statistics) <= 3, 1, 2)
  p <- ggplot(data = subset(df, variable %in% statistics), 
              aes(x = arm, y = value, fill = method)) +
    stat_summary(geom = "bar", fun = mean, position = "dodge") +
    stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") +
    facet_wrap(. ~ variable, scales = "free", ncol = ncol) +
    ggtitle("Inference (arm - arm 1)")
}
