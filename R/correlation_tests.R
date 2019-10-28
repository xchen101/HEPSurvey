#### set p-value threashold at 0.01#

# OS/OS attitude ------------------------------------------------------------------------------------------------------------------------------------------

# Association between seniority and preception of OS and OA#
CrossTable(completesubmission$D3, completesubmission$p2q1_OS1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q1_OS2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q1_OS3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q2_OA1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p2q2_OA2, chisq = TRUE)
# no correlation

# Association between field and preception of OS and OA#
CrossTable(completesubmission$D4, completesubmission$p2q1_OS1, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p2q1_OS2, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p2q1_OS3, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p2q2_OA1, chisq = TRUE)
CrossTable(completesubmission$D4, completesubmission$p2q2_OA2, chisq = TRUE)
# no correlation

# Association between gender and preception of OS and OA#
CrossTable(completesubmission$D1, completesubmission$p2q1_OS1, chisq = TRUE)
CrossTable(completesubmission$D1, completesubmission$p2q1_OS2, chisq = TRUE)
CrossTable(completesubmission$D1, completesubmission$p2q1_OS3, chisq = TRUE)
CrossTable(completesubmission$D1, completesubmission$p2q2_OA1, chisq = TRUE)
CrossTable(completesubmission$D1, completesubmission$p2q2_OA2, chisq = TRUE)
# no correlation


# interaction with OD -------------------------------------------------------------------------------------------------------------------------------------
# Association between experimentalists' seniority and OD practice#
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD1, chisq = TRUE) #p =  0.0001664108
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD2, chisq = TRUE) #p =  0.0002161671 
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD4, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1E_OD5, chisq = TRUE)

# Association between size of experimental collaboration and OD practice#
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD1, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD2, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD3, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD4, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q1E_OD5, chisq = TRUE)
# no correlation

# Association between theorists' seniority and OD practice#
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD6, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD4, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q1T_OD5, chisq = TRUE)
# no correlation


# Attitude to OD ------------------------------------------------------------------------------------------------------------------------------------------
#Association between experimentalists' seniority and OD attitude#
CrossTable(completesubmission$D3, completesubmission$p3q2E_OD_E1, chisq = TRUE) #p =  0.001831989 
CrossTable(completesubmission$D3, completesubmission$p3q2E_OD_E2, chisq = TRUE) #p =  4.250412e-05 
CrossTable(completesubmission$D3, completesubmission$p3q2E_OD_E3, chisq = TRUE) #p =  0.002640031
CrossTable(completesubmission$D3, completesubmission$p3q2E_OD_E4, chisq = TRUE)

#Association between size of experimental collaboration and OD attitude#
CrossTable(completesubmission$D4_1, completesubmission$p3q2E_OD_E1, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q2E_OD_E2, chisq = TRUE)
CrossTable(completesubmission$D4_1, completesubmission$p3q2E_OD_E3, chisq = TRUE) 
CrossTable(completesubmission$D4_1, completesubmission$p3q2E_OD_E4, chisq = TRUE)
# no correlation

#Association between theorists' seniority and OD attitude#
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E1, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E2, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E3, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E5, chisq = TRUE)
CrossTable(completesubmission$D3, completesubmission$p3q2T_OD_E4, chisq = TRUE)
# no correlation


# when to share -------------------------------------------------------------------------------------------------------------------------------------------


