for (ch in levels(preds$ypred)) print(ch)
very.neg.count = 0
very.not.neg.count = 0
very.pos.count = 0
very.not.pos.count = 0
neg.count = 0
not.neg.count = 0
pos.count = 0
not.pos.count = 0
neutral.count = 0
not.neutral.count = 0
itr = 1
for (ch in as.character(preds$ypred)){
  if(ch == "Very negative" && datatest$FactorOrdinalETL[itr] == "Very negative"){
    very.neg.count = very.neg.count + 1
  }
  
  if(ch != "Very negative" && datatest$FactorOrdinalETL[itr] == "Very negative"){
    very.not.neg.count =very.not.neg.count + 1
  }
  
  if(ch == "Very positive" && datatest$FactorOrdinalETL[itr] == "Very positive"){
    very.pos.count = very.pos.count + 1
  }
  
  if(ch != "Very positive" && datatest$FactorOrdinalETL[itr] == "Very positive"){
    very.not.pos.count =very.not.pos.count + 1
  }
  
  if(ch == "Positive" && datatest$FactorOrdinalETL[itr] == "Positive"){
    pos.count = pos.count + 1
  }
  
  if(ch != "Positive" && datatest$FactorOrdinalETL[itr] == "Positive"){
    not.pos.count =not.pos.count + 1
  }
  
  if(ch == "Negative" && datatest$FactorOrdinalETL[itr] == "Negative"){
    neg.count = neg.count + 1
  }
  
  if(ch != "Negative" && datatest$FactorOrdinalETL[itr] == "Negative"){
    not.neg.count =not.neg.count + 1
  }
  
  if(ch == "Neutral" && datatest$FactorOrdinalETL[itr] == "Neutral"){
    neutral.count = neutral.count + 1
  }
  
  if(ch != "Very negative" && datatest$FactorOrdinalETL[itr] == "Very negative"){
    not.neutral.count =not.neutral.count + 1
  }
  itr = itr +  1
  
}
