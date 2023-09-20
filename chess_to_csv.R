chess_to_csv=function(txt_url, export_path_name){
  # chess_to_csv
  #by Jean Jimenez
  #09/20/2023 for Data 607 Proj 1
  
  #Function that takes .txt file of chess tournament notation to .csv to the specifications of the assignment
  
  #INPUTS
  
  #txt_url: url of txt file in chess notation
  
  #export_path_name:  Path and name of csv file to be exported i.e.) "/home/Data607/proj1/chess_test.csv"
  
  
  #OUTPUT
  #CSV file showing player name, player state, total points, pre rating, avg opponent pre rating

  
  #First import text file
  #will read line by line
  
  lines= readLines(txt_url)
  lines
  
  #filtering out header
  
  lines=lines[-c(1,2,3,4)]
  
  
  #making a conditional statement
  
  #If the length of the # of lines is divisible by 3,
  #take every third line (dashed) and remove it from our text
  #aka generate a all multiples of 3 till len lines
  
  
  
  if (length(lines)%%3 == 0){
    multiples_of_3=seq(3, length(lines),by=3)
    lines=lines[-c(multiples_of_3)]
  }
  
  lines
  
  #Now we only have player info, with the same info in every other line
  #the first line has the names and who is playing against/ win or lose
  #the second line has the ranking/ state
  
  #going to separate into two vectors separating two lines 
  
  line_one=c()
  line_two=c()
  id=1
  
  for(line in lines){
    if(id%%2==0){
      line_two=c(line_two,line)
    }else {
      line_one=c(line_one,line)
    }
    id=id+1
  }
  line_one
  line_two
  
  #Now line one and two are each showing the same info.
  #will make each a data frame
  # every value separated by  '|'  will get its own column
  
  split_data_one=lapply(line_one, function(x) strsplit(x, "\\|"))
  split_data_two=lapply(line_two, function(x) strsplit(x, "\\|"))
  
  #moving from list to matrix [temp] to dataframe
  
  split_data_one_mat=do.call(rbind, lapply(split_data_one, function(x) unlist(x[[1]])))
  df_one=as.data.frame(split_data_one_mat, stringsAsFactors = FALSE)
  split_data_two_mat=do.call(rbind, lapply(split_data_two, function(x) unlist(x[[1]])))
  df_two=as.data.frame(split_data_two_mat, stringsAsFactors = FALSE)
  
  #Adding column names
  col_names_1=c('player_id','name','total_pts','rnd_1_comb','rnd_2_comb','rnd_3_comb','rnd_4_comb','rnd_5_comb','rnd_6_comb','rnd_7_comb')
  colnames(df_one)=col_names_1
  col_names_2=c('state','comb_rank','idk','col_rnd_1','col_rnd_2','col_rnd_3','col_rnd_4','col_rnd_5','col_rnd_6','col_rnd_7')
  colnames(df_two)=col_names_2
  
  head(df_one)
  head(df_two)
  
  #Splitting each combined round x colomn n to get the opponent number
  #put in new dataframe
  
  df_one_to_split=c('rnd_1_comb','rnd_2_comb','rnd_3_comb','rnd_4_comb','rnd_5_comb','rnd_6_comb','rnd_7_comb')
  
  #getting only numbeer characters from each column and converting to number
  
  for (col in df_one_to_split){
    df_one[[col]]=as.numeric(gsub("[^0-9]","",df_one[[col]]))
  }
  
  head(df_one)
  
  #changing column names to be more accurate
  col_names_1=c('player_id','name','total_pts','rnd_1_op','rnd_2_op','rnd_3_op','rnd_4_op','rnd_5_op','rnd_6_op','rnd_7_op')
  colnames(df_one)=col_names_1
  
  #now splitting each combined_rank columnn to get pre rating
  
  library(stringr)
  
  df_two$comb_rank=str_extract(df_two$comb_rank, "(?<=R: )\\d+")
  df_two$comb_rank=as.numeric(df_two$comb_rank)
  col_names_2=c('state','pre_rank','idk','col_rnd_1','col_rnd_2','col_rnd_3','col_rnd_4','col_rnd_5','col_rnd_6','col_rnd_7')
  colnames(df_two)=col_names_2
  head(df_two)
  
  
  
  #Now lets combine both data frames
  #first will remove columns we dont care about
  df_two$col_rnd_1=NULL
  df_two$col_rnd_2=NULL
  df_two$col_rnd_3=NULL
  df_two$col_rnd_4=NULL
  df_two$col_rnd_5=NULL
  df_two$col_rnd_6=NULL
  df_two$col_rnd_7=NULL
  df_two$idk=NULL
  head(df_two)
  
  #combine the two data frames
  
  chess_df=cbind(df_one,df_two)
  chess_df$player_id=as.integer(chess_df$player_id)
  head(chess_df)
  
  #creating new column named avg op rank
  #making a loop that will get all the oposition ranks
  
  
  chess_df$avg_op_pre_rank = NA
  
  # Loop through each row in chess_df
  for (i in 1:nrow(chess_df)) {
    
    # extract op ID for each round
    
    op_ids = unlist(chess_df[i, grep("rnd_\\d+_op", names(chess_df))])
    
    op_ids=as.integer(op_ids)
    
    #take out NA
    
    op_ids=na.omit(op_ids)
    
    # get prerank for each op
    
    op_ranks = chess_df[chess_df$player_id %in% op_ids, "pre_rank"]
    
    #mean op pre rank
    
    chess_df$avg_op_pre_rank[i] = mean(op_ranks, na.rm = TRUE)
  }
  
  head(chess_df)
  
  #making new dataframe with required format like the assignment states
  
  to_be_exported_list=list(Player_Name=chess_df$name,
                           Player_State=chess_df$state,
                           Total_Points=chess_df$total_pts,
                           Player_Pre_Rating=chess_df$pre_rank,
                           Average_Opponent_Pre_Rating=chess_df$avg_op_pre_rank)
  to_be_exported_df=as.data.frame(to_be_exported_list)
  
  head(to_be_exported_df)
  
  #write and export as csv
  write.csv(to_be_exported_df,export_path_name, row.names=FALSE)
  
}