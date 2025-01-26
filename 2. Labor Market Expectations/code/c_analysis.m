% -------------------------------------------------------------------------
% This script:   - computes the average labor market expectations and
%                  probabilities overall and for different subsamples
%                - Computes the expectation bias
%                - contains the regression of actual labor market
%                  transitions on transition expectations
% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
% expected probability of labor market transitions in 4 months 
% -------------------------------------------------------------------------

% unemployed -> employed expectation
UE_exp = fillmissing(Data(:, col_oo1_1), 'constant', 0);  % exp. to be employed

% unemployed -> unemployed expectation
UU_exp = fillmissing(Data(:, col_oo1_5), 'constant', 0); % unemployed, looking for work

% unemployed -> nilf expectation
UN_exp = fillmissing(Data(:, col_oo1_6), 'constant', 0); % unemployed, not looking for work

% NaN for individuals not providing any probability
% for all other individuals NaN = 0 probability
UE_exp(isnan(Data(:, col_oo1_1)) & isnan(Data(:, col_oo1_2)) & ...
    isnan(Data(:, col_oo1_3)) & isnan(Data(:, col_oo1_4)) & ...
    isnan(Data(:, col_oo1_5)) & isnan(Data(:, col_oo1_6))) = NaN;

UU_exp(isnan(Data(:, col_oo1_1)) & isnan(Data(:, col_oo1_2)) & ...
    isnan(Data(:, col_oo1_3)) & isnan(Data(:, col_oo1_4)) & ...
    isnan(Data(:, col_oo1_5)) & isnan(Data(:, col_oo1_6))) = NaN;

UN_exp(isnan(Data(:, col_oo1_1)) & isnan(Data(:, col_oo1_2)) & ...
    isnan(Data(:, col_oo1_3)) & isnan(Data(:, col_oo1_4)) & ...
    isnan(Data(:, col_oo1_5)) & isnan(Data(:, col_oo1_6))) = NaN;

% NaN for employed or nilf persons - only interested in unemployed
UE_exp(empl_stat ~= 2) = NaN;
UU_exp(empl_stat ~= 2) = NaN;
UN_exp(empl_stat ~= 2) = NaN;

% -------------------------------------------------------------------------



% ------------------------------------------------------------------------
% actual transition probability U -> E
% ------------------------------------------------------------------------
date = unique(Data(:, col_date));

% initiate vector to store transition indicator
UE_prob = NaN(length(Data), 1);
%--------------------------------------------------------------------------

for i = 2:length(date)
    
    t1 = date(i);       % t + 1 (here, t + 4 months)
    t = date(i - 1);    % t

%--------------------------------------------------------------------------
    % extract data of unemployed in t and store their IDs
    unemployed_t = Data((empl_stat == 2 & Data(:,col_date) == t), :); 
    unemployed_ID = unemployed_t(:,col_userid);
%--------------------------------------------------------------------------
    % extract data of unemployed, employed, nilf in t+1
    unemployed_t1  = Data((empl_stat == 2 & Data(:,col_date) == t1),:);
    nilf_t1  = Data((empl_stat == 3 & Data(:,col_date) == t1),:);
    employed_t1  = Data((empl_stat == 1 & Data(:,col_date) == t1),:);
%--------------------------------------------------------------------------
    % identify transitions from t to t+1 
    Ut_Ut1 = ismember( unemployed_t(:,col_userid), unemployed_t1(:,col_userid) );
    Ut_NILFt1 = ismember( unemployed_t(:,col_userid), nilf_t1(:,col_userid) );
    Ut_Et1 = ismember( unemployed_t(:,col_userid), employed_t1(:,col_userid) );
%--------------------------------------------------------------------------
     % indicator = 1 for individuals transitioning from unemployment to
     % employmnt, indicator = 0 for individuals transitioning from
     % unemployment to unemployment or nilf. NaN for individuals that are
     % unobserved in t+1.

    UE_prob_t1 = NaN(size(unemployed_t,1), 1);

    UE_prob_t1(Ut_Ut1 == 1 | Ut_NILFt1 == 1 , :) = 0;
    UE_prob_t1(Ut_Et1 == 1, :) =  1;

%--------------------------------------------------------------------------      
    % store indicator for UE transition at position of the individuals' ID
    % in period t.
    % multiply by 100 to get the share in %.

    for y = 1:length(unemployed_ID)

    UE_prob(Data(:, col_userid) == unemployed_ID(y) & Data(:, col_date) == t) ...
        = UE_prob_t1(y)*100;
    
    end 
end
%--------------------------------------------------------------------------  




% -------------------------------------------------------------------------
% Calculate weighted expected UE probability, share of actual UE transitions, 
% bias and respective standard errors and confidence bands for all subgroups
% indicated in the "i_groups" matrix.
%
% Note: Since the indicator for transitioning from unemployment in t to
% employment in t+1 is stored "in period" t, the weights of the individual
% from period t are applied. As the expectations for transition in t+1 are
% are formed in period t, the individuals transitioning in t+1 should be 
% representative from today's (period t's) perspective.
% -------------------------------------------------------------------------
% initiate matrix for storing results
results = zeros(width(i_groups), 10);

% columns:
mean_prob = 1;
mean_exp = 2; 
n_prob = 3; 
n_exp = 4; 
sem_prob = 5; 
sem_exp = 6; 
mean_bias = 7;
sem_bias = 8;
lb_bias = 9;
ub_bias = 10;

% -------------------------------------------------------------------------
% loop through all subgroups
for i = 1:length(results)

% extract expectations, probabilities and weights for selected individuals
% that are part of the current subgroup
prob = UE_prob .* i_groups(:,i); % indicator for actual UE transitions (probability)
exp = UE_exp .* i_groups(:, i);  % expected probability
wght = Data(:, col_weight) .* i_groups(:, i); % survey weight

% -------------------------------------------------------------------------
% calculate weighted mean 
results(i, 1) = sum(prob .* wght, 'omitNaN') ./ ...
                sum(wght(~isnan(prob)), 'omitNaN'); % UE transition probability

results(i, 2) = sum(exp .* wght, 'omitNaN') ./ ...
                sum(wght(~isnan(exp)), 'omitNaN');  % UE transition expectation
% -------------------------------------------------------------------------
% number of observations
results(i, 3) = sum(~isnan(prob) .* i_groups(:, i));
results(i, 4) = sum(~isnan(exp).* i_groups(:, i));
% -------------------------------------------------------------------------
% weighted standard deviations and standard errors
std_prob = sqrt( sum(wght .* (prob - results(i,mean_prob)).^2 , 'omitNaN') ...
                / sum(wght(~isnan(UE_prob)), 'omitNaN'));
results(i, 5) = sqrt(std_prob^2/results(i, n_prob)); % UE transition probability

std_exp = sqrt( sum(wght .* (exp - results(i,mean_exp)).^2 , 'omitNaN') ...
                / sum(wght(~isnan(UE_exp)), 'omitNaN'));
results(i, 6) = sqrt(std_exp^2/results(i,n_exp)); % UE transition expectation
% -------------------------------------------------------------------------
% weighted mean bias
results(i, 7) = results(i,mean_exp) - results(i,mean_prob);
% -------------------------------------------------------------------------
% weighted standard error of mean bias
results(i, 8) = sqrt( (std_prob^2 / results(i,n_prob)) + ...
                (std_exp^2 / results(i,n_exp)) );
% -------------------------------------------------------------------------
% upper and lower bound of bias (95% significance level)
results(i, 9) = results(i, mean_bias) - 1.96*results(i, sem_bias);
results(i, 10) = results(i, mean_bias) + 1.96*results(i, sem_bias);
end
% -------------------------------------------------------------------------
% display total results 
disp('**************************************************');
disp('Average (expected) transitions from unemployed to employed');

disp('Expected        SE      N');
disp([results(col_total, 2), results(col_total, 6), results(col_total, 4)]);
disp('Actual          SE      N');
disp([results(col_total, 1), results(col_total, 5), results(col_total, 3)]);
disp('Bias          SE');
disp([results(col_total, 7), results(col_total, 8)]);
disp('**************************************************');
%--------------------------------------------------------------------------


% -------------------------------------------------------------------------
% Weighted regression: predicting actual UE transitions with expected
% probability
% -------------------------------------------------------------------------
% design matrix
X = UE_exp; 
X_FE = [UE_exp, i_groups(:, 19:25)]; % FE: year dummies 2015-2021 (base = 2014)

% dependent variable: indicator for actual UE transition * 100
y = UE_prob;

mdl = fitlm(X, y, 'Weights', Data(:, col_weight)); % without year FE

disp('******************************************************************');
disp('Effect of employment expectation on actual transition');
disp(mdl.Coefficients)
disp('******************************************************************');

mdl_FE = fitlm(X_FE, y, 'Weights', Data(:, col_weight)); % with year FE

disp('******************************************************************');
disp('Effect of employment expectation on actual transition (incl year FEs)');
disp(mdl_FE.Coefficients)
disp('******************************************************************');
% -------------------------------------------------------------------------

