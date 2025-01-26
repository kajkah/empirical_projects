% -------------------------------------------------------------------------
% FINAL PROJECT - Macroeconomics with Micro Data
% Project title: Labor Market Expectations
% 
% Author: Lea Best (best@ifo.de) & Karolina Hozova (s7809268@stud.uni-frankfurt.de)
% Date:   08.02.2024 
% -------------------------------------------------------------------------
% This script:   - runs all the matlab code of this project
%                - stores the figures in the output folder
%                - displays non-graphical results
% -------------------------------------------------------------------------

%-------------------------------------------------------------------------
clear % clear workspace
% -------------------------------------------------------------------------

%-------------------------------------------------------------------------
% SET PATH
% You need to change the base path to folder in which data, code, and output 
% folder are stored.
%-------------------------------------------------------------------------
BasePath = '\Users\karolina\Macro with Data\Final_Project\Best-Hozova';


% Specify paths for code, data, and output
CodePath = fullfile(BasePath, '\code');
DataPath = fullfile(BasePath, '\data');
OutputPath = fullfile(BasePath, '\output');
% -------------------------------------------------------------------------


%-------------------------------------------------------------------------
% Run main scripts
%-------------------------------------------------------------------------

run(fullfile(CodePath, '\b_prep.m'));        % data preparation
run(fullfile(CodePath, '\c_analysis.m'));    % calculations and regressions
run(fullfile(CodePath, '\d_graphs.m'));      % graphs
%------------------------------------------------------------------------
