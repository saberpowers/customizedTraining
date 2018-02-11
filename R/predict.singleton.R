predict.singleton <-
function(object, type = c('response', 'class', 'nonzero'), ...)
{
  type = match.arg(type)
  if (type == 'nonzero') return(NULL)
  return(object[[type]])
}
