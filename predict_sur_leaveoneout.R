predictSUR_leave <- function(modlist, basedata, basedata_means, newdata, terms, acres){
    
    outlist <- list()
    surmods <- list(cotton = modlist[[2]], hay = modlist[[3]], soybean = modlist[[4]], wheat = modlist[[5]])
    mod <- systemfit(surmods, data = basedata, method = "SUR")
    mod$effects <- list(corn.effect = basedata_means$z_corn_a,
                    cotton.effect = basedata_means$z_cotton_a,
                    hay.effect = basedata_means$z_hay_a,
                    soybean.effect = basedata_means$z_soybean_a,
                    wheat.effect = basedata_means$z_wheat_a)
    pmod <- predictSUR(mod, newdata, var.terms = terms)
    
    pmod$cotton_predict <- pnorm(pmod$cotton_predict + resid(mod)[[1]] + mod$effects$cotton.effect)*1.00101 - 0.001
    pmod$hay_predict <- pnorm(pmod$hay_predict + resid(mod)[[2]] + mod$effects$hay.effect)*1.00101 - 0.001
    pmod$soybean_predict <- pnorm(pmod$soybean_predict + resid(mod)[[3]] + mod$effects$soybean.effect)*1.00101 - 0.001
    pmod$wheat_predict <- pnorm(pmod$wheat_predict + resid(mod)[[4]] + mod$effects$wheat.effect)*1.00101 - 0.001
    pmod$corn_predict <- 1 - rowSums(pmod[, 1:4])
    pmod <- apply(pmod, 2, function(x) ifelse(x < 0, 0, x))
    pmod <- apply(pmod, 2, function(x) ifelse(x > 1, 1, x))
    pmod[, 1:5] <- pmod[, 1:5] / rowSums(pmod[, 1:5])
    outlist[[1]] <- as.data.frame(pmod)

    # Remove Cotton
    surmods <- list(corn = modlist[[1]], hay = modlist[[3]], soybean = modlist[[4]], wheat = modlist[[5]])
    mod <- systemfit(surmods, data = basedata, method = "SUR")
    mod$effects <- list(corn.effect = basedata_means$z_corn_a,
                        cotton.effect = basedata_means$z_cotton_a,
                        hay.effect = basedata_means$z_hay_a,
                        soybean.effect = basedata_means$z_soybean_a,
                        wheat.effect = basedata_means$z_wheat_a)
        
        pmod <- predictSUR(mod, newdata, var.terms = terms)
        
        pmod$corn_predict <- pnorm(pmod$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
        pmod$hay_predict <- pnorm(pmod$hay_predict + resid(mod)[[2]] + mod$effects$hay.effect)*1.00101 - 0.001
        pmod$soybean_predict <- pnorm(pmod$soybean_predict + resid(mod)[[3]] + mod$effects$soybean.effect)*1.00101 - 0.001
        pmod$wheat_predict <- pnorm(pmod$wheat_predict + resid(mod)[[4]] + mod$effects$wheat.effect)*1.00101 - 0.001
        pmod$cotton_predict <- 1 - rowSums(pmod[, 1:4])
            pmod <- apply(pmod, 2, function(x) ifelse(x < 0, 0, x))
    pmod <- apply(pmod, 2, function(x) ifelse(x > 1, 1, x))
    pmod[, 1:5] <- pmod[, 1:5] / rowSums(pmod[, 1:5])
        outlist[[2]] <- as.data.frame(pmod)

        surmods <- list(corn = modlist[[1]], cotton = modlist[[2]], soybean = modlist[[4]], wheat = modlist[[5]])
        mod <- systemfit(surmods, data = basedata, method = "SUR")
        mod$effects <- list(corn.effect = basedata_means$z_corn_a,
                        cotton.effect = basedata_means$z_cotton_a,
                        hay.effect = basedata_means$z_hay_a,
                        soybean.effect = basedata_means$z_soybean_a,
                        wheat.effect = basedata_means$z_wheat_a)
        
        pmod <- predictSUR(mod, newdata, var.terms = terms)
        pmod$corn_predict <- pnorm(pmod$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
        pmod$cotton_predict <- pnorm(pmod$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
        pmod$soybean_predict <- pnorm(pmod$soybean_predict + resid(mod)[[3]] + mod$effects$soybean.effect)*1.00101 - 0.001
        pmod$wheat_predict <- pnorm(pmod$wheat_predict + resid(mod)[[4]] + mod$effects$wheat.effect)*1.00101 - 0.001
        pmod$hay_predict <- 1 - rowSums(pmod[, 1:4])
            pmod <- apply(pmod, 2, function(x) ifelse(x < 0, 0, x))
    pmod <- apply(pmod, 2, function(x) ifelse(x > 1, 1, x))
    pmod[, 1:5] <- pmod[, 1:5] / rowSums(pmod[, 1:5])
        outlist[[3]] <- as.data.frame(pmod)

        
        # Remove Soybean
        surmods <- list(corn = modlist[[1]], cotton = modlist[[2]], hay = modlist[[3]], wheat = modlist[[5]])
        mod <- systemfit(surmods, data = basedata, method = "SUR")
        mod$effects <- list(corn.effect = basedata_means$z_corn_a,
                        cotton.effect = basedata_means$z_cotton_a,
                        hay.effect = basedata_means$z_hay_a,
                        soybean.effect = basedata_means$z_soybean_a,
                        wheat.effect = basedata_means$z_wheat_a)
        
        pmod <- predictSUR(mod, newdata, var.terms = terms)
        pmod$corn_predict <- pnorm(pmod$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
        pmod$cotton_predict <- pnorm(pmod$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
        pmod$hay_predict <- pnorm(pmod$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.00101 - 0.001
        pmod$wheat_predict <- pnorm(pmod$wheat_predict + resid(mod)[[4]] + mod$effects$wheat.effect)*1.00101 - 0.001
        pmod$soybean_predict <- 1 - rowSums(pmod[, 1:4])
            pmod <- apply(pmod, 2, function(x) ifelse(x < 0, 0, x))
    pmod <- apply(pmod, 2, function(x) ifelse(x > 1, 1, x))
    pmod[, 1:5] <- pmod[, 1:5] / rowSums(pmod[, 1:5])
        outlist[[4]] <- as.data.frame(pmod)

        # Remove Wheat
        surmods <- list(corn = modlist[[1]], cotton = modlist[[2]], hay = modlist[[3]], soybean = modlist[[4]])
        mod <- systemfit(surmods, data = basedata, method = "SUR")
        mod$effects <- list(corn.effect = basedata_means$z_corn_a,
                        cotton.effect = basedata_means$z_cotton_a,
                        hay.effect = basedata_means$z_hay_a,
                        soybean.effect = basedata_means$z_soybean_a,
                        wheat.effect = basedata_means$z_wheat_a)
        
        pmod <- predictSUR(mod, newdata, var.terms = terms)
        pmod$corn_predict <- pnorm(pmod$corn_predict + resid(mod)[[1]] + mod$effects$corn.effect)*1.00101 - 0.001
        pmod$cotton_predict <- pnorm(pmod$cotton_predict + resid(mod)[[2]] + mod$effects$cotton.effect)*1.00101 - 0.001
        pmod$hay_predict <- pnorm(pmod$hay_predict + resid(mod)[[3]] + mod$effects$hay.effect)*1.00101 - 0.001
        pmod$soybean_predict <- pnorm(pmod$soybean_predict + resid(mod)[[4]] + mod$effects$soybean.effect)*1.00101 - 0.001
        pmod$wheat_predict <- 1 - rowSums(pmod[, 1:4])
            pmod <- apply(pmod, 2, function(x) ifelse(x < 0, 0, x))
    pmod <- apply(pmod, 2, function(x) ifelse(x > 1, 1, x))
    pmod[, 1:5] <- pmod[, 1:5] / rowSums(pmod[, 1:5])
        outlist[[5]] <- as.data.frame(pmod)
        # print(i)

# rowmeans
outdat <- data.frame(corn_predict = rowMeans(data.frame(x1 = outlist[[1]]$corn_predict,
                                                        x2 = outlist[[2]]$corn_predict,
                                                        x3 = outlist[[3]]$corn_predict,
                                                        x4 = outlist[[4]]$corn_predict,
                                                        x5 = outlist[[5]]$corn_predict)),
                     cotton_predict = rowMeans(data.frame(x1 = outlist[[1]]$cotton_predict,
                                                        x2 = outlist[[2]]$cotton_predict,
                                                        x3 = outlist[[3]]$cotton_predict,
                                                        x4 = outlist[[4]]$cotton_predict,
                                                        x5 = outlist[[5]]$cotton_predict)),
                     hay_predict = rowMeans(data.frame(x1 = outlist[[1]]$hay_predict,
                                                        x2 = outlist[[2]]$hay_predict,
                                                        x3 = outlist[[3]]$hay_predict,
                                                        x4 = outlist[[4]]$hay_predict,
                                                        x5 = outlist[[5]]$hay_predict)),
                     soybean_predict = rowMeans(data.frame(x1 = outlist[[1]]$soybean_predict,
                                                        x2 = outlist[[2]]$soybean_predict,
                                                        x3 = outlist[[3]]$soybean_predict,
                                                        x4 = outlist[[4]]$soybean_predict,
                                                        x5 = outlist[[5]]$soybean_predict)),
                     wheat_predict = rowMeans(data.frame(x1 = outlist[[1]]$wheat_predict,
                                                        x2 = outlist[[2]]$wheat_predict,
                                                        x3 = outlist[[3]]$wheat_predict,
                                                        x4 = outlist[[4]]$wheat_predict,
                                                        x5 = outlist[[5]]$wheat_predict)))
outdat <- apply(outdat, 2, function(x) x*acres)
# Remove values not in [0,1] and then weight to sum rows to one
# outdat <- apply(outdat, 2, function(x) ifelse(x < 0, 0, x))
# outdat <- apply(outdat, 2, function(x) ifelse(x > 1, 1, x))
# outdat[, 1:5] <- outdat[, 1:5] / rowSums(outdat[, 1:5])
# outdat <- rowSums(outdat[,1:5])
return(data.frame(outdat))
}

# a <- predictSUR_leave(modlist = modlist, newdata = newdata, basedata_means = basedata_means)
# a
# 
# head(outdat)
# head(old.outdat)
# sum(outdat$cotton_predict)
# sum(old.outdat$cotton_predict)
# sum(old.outdat)
# sum(c(cropdat$p_corn_a, cropdat$p_cotton_a, cropdat$p_hay_a, cropdat$p_soybean_a, cropdat$p_wheat_a))

#old.outdat <- outdat
