% NWTC_Library source files:
sourceDir = '../../source/';
files = {
'NWTC_Library'
... %'DoubPrec'
'SingPrec'
'NWTC_Base'
'NWTC_IO'
'NWTC_Num'
... %'SysGnuLinux'
... %'SysGnuWin'
... %'SysIFL'
'SysIVF'
'NWTC_Library_Types'
... %'SysIVF_Labview'
... %'SysMatlab'
'ModMesh'
... %'ModMesh_Types'
... %'ModMesh_Mapping'
};


fout(2) = fopen( '..\NWTC_Library_Routines.txt', 'wt' );
if fout(2) < 1
    disp( ['could not open ' fout(2) ' for writing.']);
    return
end

%%
for i=1:length(files);
    inputFileName  = strcat(sourceDir, files{i}, '.f90');    
    
    outputFileName = strcat(files{i}, '.txt');
    fout(1) = fopen( outputFileName, 'wt' );
    
    if fout(1) < 1
        disp( ['could not open ' fout(1) ' for writing.']);
        continue;
    end
    
    GenListOfRoutines(inputFileName,fout);
    fclose(fout(1));
end

fclose(fout(2));