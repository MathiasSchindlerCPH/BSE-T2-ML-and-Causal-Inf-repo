def adj_form(df, adj_var):
  '''
  Calculate Adjustment Formula from a dataframe and a confounding variable
  
  Author: Mathias Schindler
  '''
  
  # Probabilities of confounder
  p1 = df[adj_var].mean()
  p0 = 1-p1

  # Conditional expectations of outcomes
  EOT1C1 = df[(df['T'] == 1) & (df[adj_var] == 1)]['O'].mean()
  EOT1C0 = df[(df['T'] == 1) & (df[adj_var] == 0)]['O'].mean()

  EOT0C1 = df[(df['T'] == 0) & (df[adj_var] == 1)]['O'].mean()
  EOT0C0 = df[(df['T'] == 0) & (df[adj_var] == 0)]['O'].mean()

  # Adjustment Formula terms
  EOT1 = EOT1C1*p1 + EOT1C0*p0
  EOT0 = EOT0C1*p1 + EOT0C0*p0

  # Adjustment Formula
  adf_ate = EOT1 - EOT0

  return adf_ate