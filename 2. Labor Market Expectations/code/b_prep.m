% -------------------------------------------------------------------------
% This script:   - loads the merged micro dataset
%                - prepares the data
%                - applies the sample selection
% -------------------------------------------------------------------------


%-------------------------------------------------------------------------
cd(DataPath); % change directory to data path
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% Load merged data
% -------------------------------------------------------------------------
Data  = readtable('data_merged.csv');        

varTable    = Data.Properties.VariableNames'; % Variable names in table

numVar = numel(varTable);                  % Number of variables in dataset

% Create variable representing column number for each variable
for i = 1:numVar
    assignin('base', ['col_', varTable{i}], i )
end

Data = table2array(Data);              % Transform table into array
% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
% Prepare characteristics only answered at first participation
% -------------------------------------------------------------------------
% characteristics are only answered by new respondents
% impute later observations with first observation by individual

uniqueUserID = unique(Data(:, col_userid)); % unique IDs

for i = 1:length(uniqueUserID)
    userID = uniqueUserID(i);
    i_FirstValue = find(Data(:, col_userid) == userID, 1, 'first');

    for y = col_QNUM1:col_Q47 % loop over all columns containing characteristics
    char = Data(i_FirstValue, y);
    Data(Data(:, col_userid)  == userID, y) = char;
    end
end
% -------------------------------------------------------------------------
% convert date to string
dateStr = arrayfun(@num2str, Data(:, col_date), 'UniformOutput', false);

% extract year and month
year = cellfun(@(x) str2double(x(1:4)), dateStr);
month = cellfun(@(x) str2double(x(5:6)), dateStr);
% -------------------------------------------------------------------------
% impute age based on years passed since first participation
for i = 1:length(uniqueUserID)
    userID = uniqueUserID(i);
    
    % Find the indices for the current userid
   index = Data(:, col_userid) == userID;
    
    % impute age with first observation by individual
    Data(index, col_Q32) = Data(index, col_Q32)  + ...
       (year(index) - year(find(index, 1, 'first')));
end
% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
% Sample selection
% -------------------------------------------------------------------------
% Keep only observations from the labor market survey (based on merge
% variable from merging the core to the labor market module in stata, 
% merge = 3 -> observation is part of both modules)

merge = Data(:, col_merge); % extract merge variable
Data(merge ~= 3, :) = [];   % drop not-merged observations

year_clean = year;          % year variable with the same sample selection
year_clean(merge ~= 3) = [];


% keep only individuals between 25-60
year_clean(Data(:, col_Q32) < 25 | Data(:, col_Q32) > 60, :) = [];
Data(Data(:, col_Q32) < 25 | Data(:, col_Q32) > 60, :) = [];

% keep only non-students
year_clean(Data(:, col_Q10_8) == 1, :) = [];
Data(Data(:, col_Q10_8) == 1, :) = [];

% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
% Generate labor market status variable
% Classification is based on Balleer et al. (2021)
% -------------------------------------------------------------------------
empl_stat = NaN(length(Data), 1);

empl_stat(Data(:, col_Q10_6) == 1 | Data(:, col_Q10_7) == 1 | (Data(:, col_Q10_3) == 1 & Data(:, col_js5) == 2)) = 3; % nilf
empl_stat(Data(:, col_Q10_4) == 1 | (Data(:, col_Q10_3) == 1 & Data(:, col_js5) == 1)) = 2; % unemployed 
empl_stat(Data(:, col_Q10_1) == 1 | Data(:, col_Q10_2) == 1 | Data(:, col_Q10_5) == 1) = 1; % employed 

% -------------------------------------------------------------------------
% Generate matrix containing indicators for various characteristics and
% time periods to loop through in later heterogeneity analysis
% -------------------------------------------------------------------------
i_groups = zeros(length(Data), 25);

% column names
col_total = 1;
col_educ_1 = 2; 
col_educ_2 = 3;
col_female_1 = 4;
col_female_0 = 5;
col_age_1 = 6;
col_age_2 = 7;
col_married_1 = 8;
col_married_0 = 9;
col_white_1 = 10;
col_white_0 = 11;
col_covid_1 = 12;
col_covid_0 = 13;
col_num_1 = 14;
col_num_0 = 15;
col_longU_1 = 16;
col_longU_0 = 17;
% year indicators 2014 - 2022: columns 18:26

% full sample
i_groups(:, col_total) = 1;

% education
i_groups(Data(:, col_Q36) >= 1 & Data(:, col_Q36) <= 4, col_educ_1) = 1; % less than Bachelor
i_groups(Data(:, col_Q36) >= 5 & Data(:, col_Q36) < 9, col_educ_2) = 1; % Bachelor degree

% age 
i_groups(Data(:, col_Q32) >= 25 & Data(:, col_Q32) <= 39, col_age_1) = 1; % 25-39
i_groups(Data(:, col_Q32) >= 40 & Data(:, col_Q32) <= 60, col_age_2) = 1; % 40-60

% gender
i_groups(Data(:, col_Q33) == 1, col_female_1) = 1; % female
i_groups(Data(:, col_Q33) == 2, col_female_0) = 1; % male

% marital status
i_groups(Data(:, col_Q38) == 1, col_married_1) = 1; % married/ living with partner
i_groups(Data(:, col_Q38) == 2, col_married_0) = 1; % single

% race
i_groups(Data(:, col_Q35_1) == 1, col_white_1) = 1; % white
i_groups((Data(:, col_Q35_2) == 1 | ...
    Data(:, col_Q35_3) == 1 | ...
    Data(:, col_Q35_4) == 1 | ...
    Data(:, col_Q35_5) == 1 | ...
    Data(:, col_Q35_6) == 1) & (Data(:, col_Q35_6) ~= 1), col_white_0) = 1; % non-white

% pre- /post-Covid 
% Note: Using 201911 as cutoff as in this period expectations are formed
% for 202003 when Covid already hitted. So exectations from 201911 may be
% biased already because of the unexpected Covid-Shock.
i_groups(Data(:, col_date) < 201911, col_covid_0) = 1; % pre-Covid
i_groups(Data(:, col_date) >= 201911, col_covid_1) = 1; % post-Covid

% numeracy
i_groups(Data(:, col_QNUM3) == 10 &  Data(:, col_QNUM5) == 100 &  Data(:, col_QNUM6) == 5 , col_num_1) = 1; % high numeracy
i_groups(Data(:, col_QNUM3) ~= 10 | Data(:, col_QNUM5) ~= 100 |  Data(:, col_QNUM6) ~= 5 , col_num_0) = 1; % low numeracy

% job search duration
i_groups(Data(:, col_js7) >= 18, col_longU_1) = 1; % more than 18 weeks searching
i_groups(Data(:, col_js7) < 18, col_longU_0) = 1; % less than 18 weeks searching

% by year 
unique_year = unique(year_clean);
for t = 1:length(unique_year)
    i = t + 17; % start in row 18
    i_groups(year_clean == unique_year(t), i) = 1;
end
%--------------------------------------------------------------------------


%--------------------------------------------------------------------------
% variables
%--------------------------------------------------------------------------
% userid = individual ID
% date = YYYYMM
% merge = merge indicator of merging labor to core survey module (3 =
% merged)

% Labor market survey
% js5 = anything done to search for work in past 4 weeks (1 - yes, 2 - no)
% js7 = duration of job search in weeks
% oo1_1 = expectation to be employed in 4 months (in %)
% oo1_2 = expectation to be employed with same employer in 4 months (in %)
% oo1_3 = expectation to be employed with different employer in 4 months (in %)
% oo1_4 = expectation to be self-employed in 4 months (in %)
% oo1_5 = expectation to be unempl. looking for work in 4 months (in %)
% oo1_6 = expectation to be unempl. not looking for work in 4 months (in %)
% oo1_6 = expectation to be unempl. not looking for work in 4 months (in %)

% Core survey
% Q10_1 = working full-time (1 = yes)
% Q10_2 = working part-time (1 = yes)
% Q10_3 = not working, would like to work (1 = yes)
% Q10_4 = temporarily laid off (1 = yes)
% Q10_5 = sick or other leave (1 = yes)
% Q10_6 = disabled, unable to work (1 = yes)
% Q10_7 = restired (1 = yes)
% Q10_8 = in school or training (1 = yes)
% Q10_9 = other (1 = yes)
% Q10_10 = other specified 
% QNUM1-9 = questions assessing numeracy
% Q32 = current age
% Q33 = gender (1 = female, 2 = male)
% Q34 = Hispanic, Latino or Spanish origin (1 = yes, 2 = no)
% Q35_1 = White (1 = yes)
% Q35_2 = Black or African American (1 = yes)
% Q35_3 = American Indian or Alaska Native (1 = yes)
% Q35_4 = Asian (1 = yes)
% Q35_5 = Native Hawaiian or Other Pacific Islander (1 = yes)
% Q35_6 = Other 
% Q36 =  highest level of school (at least Bachelors' degree >= 5)
% Q38 =  marital status (1 = married/living with partner, 2 = single)
% Q47 = hh income (in categories)
%--------------------------------------------------------------------------



   