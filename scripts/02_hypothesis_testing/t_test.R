#do the t-test with col age & duration 
#reason we use t-test its because 2 sample quantitative
t.test(yes_group$age, no_group$age, var.equal = FALSE)
t.test(yes_group$duration, no_group$duration, var.equal = FALSE)
t.test(yes_group$campaign, no_group$campaign, var.equal = FALSE)
t.test(yes_group$pdays, no_group$pdays, var.equal = FALSE)
t.test(yes_group$previous, no_group$previous, var.equal = FALSE)
t.test(yes_group$emp.var.rate, no_group$emp.var.rate, var.equal = FALSE)
t.test(yes_group$cons.price.idx, no_group$cons.price.idx, var.equal = FALSE)
t.test(yes_group$cons.conf.idx, no_group$cons.conf.idx, var.equal = FALSE)
t.test(yes_group$euribor3m, no_group$euribor3m, var.equal = FALSE)
t.test(yes_group$nr.employed, no_group$nr.employed, var.equal = FALSE)
