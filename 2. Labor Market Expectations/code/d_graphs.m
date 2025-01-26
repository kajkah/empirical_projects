% -------------------------------------------------------------------------
% This script:   - creates all graphs that are part of the presentation
%                - stores them as jpg to the output folder
% -------------------------------------------------------------------------

%--------------------------------------------------------------------------
cd(OutputPath); % change directory to output path
% -------------------------------------------------------------------------

% -------------------------------------------------------------------------
% histogram of raw employment and unemployment expectations
% -------------------------------------------------------------------------
% UE expectations only
figure;
histogram(UE_exp, 'NumBins', 20, 'FaceColor', [0.105, 0.211, 0.627], 'Orientation', 'horizontal') % UE
xlabel('Number of Observations', 'FontSize', 14); % y title
ylabel('Expected Employment Probability in %', 'FontSize', 14); % x title
xlim([0, 160]); % y axis
set(gca, 'FontSize', 14);  % adjust axis font size
saveas(gcf, 'hist_UE_exp.jpg'); % save to jpg

% -------------------------------------------------------------------------

% UE and UU expectations 
figure;
histogram(UE_exp, 'NumBins', 20, 'FaceColor', [0.105, 0.211, 0.627],'Orientation', 'horizontal') % UE
hold on;
histogram(UU_exp, 'NumBins', 20, 'FaceColor', [0.8, 0.8, 0],'Orientation', 'horizontal') % UU
hold off;
xlabel('Number of Observations', 'FontSize', 14); % y title
ylabel('Expected Transition Probability in %', 'FontSize', 14); % x title
xlim([0, 160]); % y axis
legend({'UE', 'UU'},  'Location', 'southeast', 'Box', 'off') % legend
set(gca, 'FontSize', 14);  % adjust axis font size
saveas(gcf, 'hist_UE_UU_exp.jpg'); % save to jpg
% -------------------------------------------------------------------------



% -------------------------------------------------------------------------
% Time series graph
% -------------------------------------------------------------------------
results_ByYear =  results(18:26,:); % extract results by year

% UE expectation only 
figure;
plot(unique_year, results_ByYear(:,2), 'LineWidth', 3, 'Color', [0.105, 0.211, 0.627]);
ylabel('Expected Employment Probability in %', 'FontSize', 14); % y title
xlabel('Year', 'FontSize', 14);  % x title
ylim([0, 100]); % y axis
grid on; % add grid
set(gca, 'FontSize', 14);  % adjust axis font size
saveas(gcf, 'ts_exp.jpg'); % save to jpg

% -------------------------------------------------------------------------

% UE expectation and share of actual UE transitions 
figure;
plot(unique_year, results_ByYear(:,1), 'k--', 'LineWidth', 3, 'Color',[0, 0, 0] );
hold on;
plot(unique_year, results_ByYear(:,2), 'LineWidth', 3, 'Color',[0.105, 0.211, 0.627]);
hold off;
legend({'Actual', 'Expected'}, 'Box', 'off',  'Location', 'northwest') % legend
ylabel('Transition Probability in %', 'FontSize', 14); % y title
xlabel('Year', 'FontSize', 14);  % x title
ylim([0, 100]); % y axis
grid on; % add grid
set(gca, 'FontSize', 14);  % adjust axis font size
saveas(gcf, 'ts_exp_prob.jpg'); % save to jpg
% -------------------------------------------------------------------------


% -------------------------------------------------------------------------
% Heterogeneity graphs
% means and 95%-confidence bands
% -------------------------------------------------------------------------
% set x-axis positions for groups
x_positions_group1 = 2 - 0.4; 
x_positions_group2 = 2 +  0.4;

% store x axis labels for all groups
label = [{'Total'},
         {'No College Degree'}
         {'College Degree'},
         {'Female'}
         {'Male'},
         {'25 - 39'},
         {'40 - 60'},
         {'Married '},
         {'Single'},
         {'White'},
         {'Non-White'},
         {'Post-Covid'},
         {'Pre-Covid'},
         {'High Numeracy'},
         {'Low Numeracy'},
         {'At least 18 weeks'},
         {'Less than 18 weeks'}];

% -------------------------------------------------------------------------
% loop through results table to plot mean biases of all groups
for i = 2:2:16 % comparing group i to group y
    y = i+1;
% -------------------------------------------------------------------------
% plot weighted mean bias with corresponding 95% confidence bands for two
% groups next to each other

figure;
% group i
errorbar(x_positions_group1, results(i, 7), results(i, 8)*1.96, 'o', 'Color', ...
    [0.105, 0.211, 0.627], 'MarkerSize', 10, 'MarkerFaceColor', [0.105, 0.211, 0.627], 'CapSize', 10, 'LineWidth', 2);
hold on;
% group y
errorbar(x_positions_group2, results(y, 7), results(y, 8)*1.96, 'o', 'Color', ...
    [0, 0, 0], 'MarkerSize', 10, 'MarkerFaceColor', [0, 0, 0], 'CapSize', 10, 'LineWidth', 2);

% add a horizontal line at y = 0
yline(0, 'k--', 'LineWidth', 1.5);

hold off;

xticks([x_positions_group1, x_positions_group2]); 
xticklabels([label(i), label(y)]); % add group labels
ylabel('Employment expectation bias (in p. p.)', 'FontSize', 14);

grid on; % add grid

xlim([1, 3]); % x axis
ylim([-5, 45]); % y axis

set(gca, 'FontSize', 14);  % set axis font size

filename = sprintf('coefplot_%d.jpg', i);
saveas(gcf, filename); % save to jpg

end
% -------------------------------------------------------------------------




