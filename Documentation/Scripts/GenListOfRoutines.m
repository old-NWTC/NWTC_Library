function GenListOfRoutines(inputFileName,outputFile)

fid =fopen(inputFileName, 'rt'); %open text input file for reading   
if fid < 0
    error(['GenListOfRoutines::could not open file: ' inputFileName]);
end

if isnumeric(outputFile)
    outputFileID1 = outputFile(1);    
    if isscalar(outputFile)
        outputFileID2 = outputFileID1;
    else
        outputFileID2=outputFile(2);
    end
    closeFiles = false;
else
    outputFileID1=fopen(outputFile,'wt'); %open text input file for writing 
    outputFileID2=outputFileID1;
    closeFiles = true;
end

Tp_Module = 1;
Tp_Procedure = 2;

%% get the values in the list

    % initialization:
foundContains = false;
nlines = 0;

ModuleName = '';
moduleID = -1;
InStructure=cell(1);

NumIn  = 0;
nProc  = 0;
nVars  = 0;
nTypes = 0;
nIntf  = 0;
moduleID  = 0;

    % read each line in the source file:
line = fgetl(fid);    
while ischar(line)
    
    nlines = nlines + 1;
    line = strtrim(line); % remove leading/trailing whitespace from the line

            
    if length(line) > 1       
        if strncmpi(line, 'RECURSIVE',9)
            line = strtrim(line(10:end));
        end
        
        
        if     strncmpi(line, '!', 1)    % comment line, so just ignore for now
            
        elseif strncmpi(line, 'MODULE', 6)  % start of a new module
            line = strtrim(line(7:end));
            ModuleName = strtok(line);
            NumIn = NumIn + 1;
            InStructure{NumIn}.type = Tp_Module;
            InStructure{NumIn}.name = ModuleName;
            
            
            [numClines, comments] = getCommentLines(fid);
            nlines = nlines + numClines;
            
            moduleID = moduleID + 1;
            modData(moduleID).comments = comments;
            modData(moduleID).name = ModuleName;
            
            
        elseif strncmpi(line, 'USE', 3)  % USE module 
            % list of dependencies            
        elseif strncmpi(line, 'IMPLICIT', 8) % we only care about this because it has :: in the line            
        elseif strncmpi(line, 'PRIVATE', 7) % we only care about this because it has :: in the line            
            % at some point, we should remove the non-public routines from
            % the description because people can't use them outside the
            % module anyway.
        elseif strncmpi(line, 'PUBLIC', 6) % we only care about this because it has :: in the line            
            
        elseif strncmpi(line,'CONTAINS',8) % subroutines contained in the file
            if InStructure{NumIn}.type == Tp_Module
                foundContains = true; %this module contains subroutines
            end
                                    
        elseif strncmpi(line, 'INTERFACE', 9) 

            if foundContains % we're in a subroutine and don't care about this one.
                
                while ischar(line) && ( ...
                        isempty(strfind( upper(line),'END')) || ...
                        isempty(strfind( upper(line),'INTERFACE')) )
                        
                    line = strtrim(fgetl(fid));
                    nlines = nlines + 1;
                end
                
            else %we do care about this one, defined in the module...
                line = strtrim(line(10:end));
                nIntf = nIntf+1;
                [intfData(nIntf).name, comment] = strtok(line,'!');
                intfData(nIntf).comment = strtrim(comment(2:end));
                intfData(nIntf).module = moduleID;
                
                nIntfMods = 0;
                while ischar(line) && ( ...
                        isempty(strfind( upper(line),'END')) || ...
                        isempty(strfind( upper(line),'INTERFACE')) )
                        
                    line = strtrim(fgetl(fid));
                    nlines = nlines + 1;

                    if ~isempty(line) && ~ strcmp(line(1), '!')
                        line=strtrim(strtok(line,'!'));
                        findx=strfind(upper(line),'PROCEDURE');
                        if strncmpi(line,'MODULE',6) && ~isempty(findx)
                            nIntfMods = nIntfMods + 1;
                            intfData(nIntf).ProcName{nIntfMods} = strtok( line((findx(1)+10):end) );
                        end
                    end
                end
            end
            
        elseif strncmpi(line, 'END', 3)  % end of module or subroutine OR TYPE
            
            line = line(4:end);
            nextWord = sscanf(line,'%s', 1);
            nextWord = strtok(nextWord,'!');
            nextWord = strtok(nextWord,'&');
            
            if isempty(nextWord) || ...
               strcmpi(nextWord,'SUBROUTINE') || ...
               strcmpi(nextWord,'FUNCTION') || ...          
               strcmpi(nextWord,'MODULE') || ...          
               strcmpi(nextWord,'TYPE') || ...          
               strcmpi(nextWord,'INTERFACE')
                    
                InStructure{NumIn} = {};
                NumIn = NumIn - 1;            
                if NumIn == 0;
                    foundContains = false; %start a new module
                    ModuleName = '';
                    moduleID = -1;
                end
            end
            
        elseif strncmpi(line, 'SUBROUTINE', 10) || strncmpi(line, 'FUNCTION', 8) 
            
            [ thisProc, line, readlines ] = parseRoutineDefinition( line, fid, moduleID );
            nlines = nlines + readlines;
            if NumIn == 0 || InStructure{NumIn}.type ~= Tp_Procedure
                nProc = nProc + 1;
                procData(nProc) = thisProc;
            end
            
            NumIn = NumIn + 1;
            InStructure{NumIn}.type = Tp_Procedure;
            InStructure{NumIn}.name = thisProc.name;
            
            continue;
             
        elseif strncmpi(line,'TYPE',4) % this could be a type definition (or not)
            
            tmpLine = strtrim(line(5:end));
            if ~strcmp(tmpLine(1),'(') %this is a defining the type
                nTypes = nTypes + 1;
                typeData(nTypes).name = line; 
                
                while ischar(line) && ( ...
                        isempty(strfind( upper(line),'END')) || ...
                        isempty(strfind( upper(line),'TYPE')) )

                    line = fgetl(fid);
                    nlines = nlines + 1;
                end
            else %this is a variable:
               
                if ~foundContains %see what variables have been defined here           
                    [variable, newlines] = parseVariableDefinition ( line, fid, moduleID );
                    nlines = nlines + newlines;
                    if ~isempty(variable)
                        nVars = nVars + 1;
                        varData(nVars) = variable;
                    end                                    
                end
                
            end

            
        else                                    
            if ~foundContains %see what variables have been defined here           
                [variable, newlines] = parseVariableDefinition ( line, fid, moduleID );
                nlines = nlines + newlines;
                if ~isempty(variable)
                    nVars = nVars + 1;
                    varData(nVars) = variable;
                end                
            end
        end
    end    
    line = fgetl(fid);    

end

fclose(fid);

if moduleID == 0
    modData = [];
end
if nVars == 0
    varData = [];
end
if nTypes == 0
    typeData = [];
end

if nIntf == 0  
    intfData = [];
    
    if nProc == 0 % if there is an interface, there has to be a procedure, so let's put this here.
        procData = [];
    end   
else
    [intfData, procData] = findInterfaceRoutines( intfData, procData );    
end


writeRoutineListing(outputFileID1, procData, intfData);
writeSummary(outputFileID2, procData, typeData, varData, intfData, modData);

if closeFiles
    fclose(outputFileID1);
    fclose(outputFileID2);
end    

return;
end

function [varData, nlines] = parseVariableDefinition ( line, fid, moduleID )
    nlines = 0; 
    
    [varLine, Comment] = strtok([' ' line],'!');
    Comment = strtrim(Comment(2:end));

                
        % look for continuation lines
    [tmpLine, remainder] = strtok([' ' varLine],'&');

    while ~isempty(remainder) && strcmp(remainder(1),'&') %this is a continuation line
        line = fgetl(fid);
        nlines = nlines+1;

        [varLine2, Comment2] = strtok([ ' ' line],'!');
        Comment = strtrim( [Comment ' ' strtrim(Comment2(2:end))] );

        tmpLine = [strtrim(tmpLine) ' ' strtrim(varLine2)];
        [tmpLine, remainder] = strtok([' ' tmpLine],'&');
        tmpLine = strtrim(tmpLine);
    end

        % parse the line for variable name, type
    [varLine, varName] = strtok(tmpLine,'::');

    if ~isempty( strtrim(varName(3:end)) )               
        varData.name    = strtrim(varName(3:end));
        varData.comment = Comment;
        varData.module  = moduleID;
        varData.type    = strtrim( strtok(varLine, ',') );
    else                    
        varData = [];
disp( varLine )                
disp(varName)
    end               

    
%bjj: parse this!!!

%                 attributes = textscan( varLine,'%s','delimiter',',');
%                 attributes = strtrim(attributes{1});
%                 
%                
%                 keepAttr = true(size(attributes));
%                 varType = attributes{1};
%                 keepAttr(1) = false;
%                 
%                 for iAtt = 1:length(attributes)
%                     if strcmpi(attributes{iAtt},'PARAMETER')
%                         [varName, paramLine] = strtok(varName,'=');
%                         varName = strtrim(varName);
%                         paramLine = strtrim(paramLine(2:end));
%                         keepAttr(iAtt) = false;
%                     elseif strcmpi(attributes{iAtt},'ALLOCATABLE')
%                         [varName, allocLine] = strtok(varName,'(');
%                         varName = strtrim(varName);
%                         keepAttr(iAtt) = false;
%                     end
%                 end
%                 
%                 
%                 [varName, initVal] = strtok(varName,'=');
%                 varName = strtrim(varName);
%                 initVal = strtrim(initVal(2:end));
%                 
%                 if length(allocLine) < 1
%                     [varName, allocLine] = strtok(varName,'(');
%                     varName = strtrim(varName);
%                 end
                
                            
    
return
end 

function [ procData, nextLine , numlines] = parseRoutineDefinition( line, fid, moduleID )
%assumptions:
%   1) only arguments and/or comments are on continuation lines

    arguments = {};
    numlines  = 0;
    nArgs     = 0;

        %remove comments
    [line, endComment{1}] = strtok([' ' line], '!' ); %add a space at the beginning just in case the line starts with !

        % determine if it has a continuation line
    line = strtrim(line); %remove trailing spaces
    HasContinuation = strcmp( line(end), '&' );
    
    while HasContinuation  %concatenate the lines, removing comments 
        
        nextLine = strtrim(fgetl(fid));        % get next line
        numlines = numlines + 1;
        [line, endComment{numlines+1}] = strtok([strtrim(line(1:(end-1))) ' ' nextLine], '!' ); 
                
            % determine if it has a continuation line
        line = strtrim(line); %remove trailing spaces
        HasContinuation = strcmp( line(end), '&' );
    end
    
    line = strtok(line,')');                 %end of arg list
    [line, args] = strtok([' ' line], '(' ); %split routine defs with arguments

    % get routine name and type:
    words = textscan(line,'%s');
    routineName = words{1}{end};
    
    findx = strfind(line,routineName);   
    routineType = line(1:(findx-1));
    
    
    % now let's parse the argument list:
    args = strrep(args(2:end),',',' '); %remove commas
    if ~isempty( args )
        [args, nArgs] = textscan(args,'%s');
        arguments= args{1};
    else
        arguments = [];
    end
        
    % let's get the comment lines, now        
    [numClines, comments, nextLine] = getCommentLines(fid);
    numlines = numlines + numClines;
    
    % put the data we found in the procData structure:
    procData.type       = strtrim(routineType);
    procData.name       = strtrim(routineName);
    procData.arguments  = arguments;
    procData.comments   = comments;
    procData.module     = moduleID;
                
return;
end

function [numlines, comments, nextLine] = getCommentLines(fid)

    nComments = 0;
    comments = {};

    % let's get the comment lines, now
    nextLine = strtrim( fgetl(fid) );
    numlines = 1;
    
    isBlank = isempty(nextLine); 
    while isBlank
        nextLine = strtrim( fgetl(fid) );
        numlines = numlines + 1;

        isBlank = isempty(nextLine);
    end
    isComment = ~isempty(nextLine) && strcmpi(nextLine(1),'!');
    while isComment
        if length(nextLine) > 1
            nComments = nComments + 1;
            comments{nComments} = nextLine(2:end);
        end            
            
        nextLine = strtrim( fgetl(fid) );
        numlines = numlines + 1;

        isComment = ~isempty(nextLine) && strcmpi(nextLine(1),'!');        
    end
        
return;
end

function [] = writeRoutineListing( fout, procData, intfData )
tab=11;

%% create list of subroutines (without comments) using INTERFACE procedures, too.  
for i=1:length(intfData)
    
    nspaces = tab-length(procData(intfData(i).ProcIndx(1)).type);
    spaces  = repmat(' ',1,nspaces);
    fprintf( fout, '   !  %s%s%s \n', procData(intfData(i).ProcIndx(1)).type, spaces, intfData(i).name );
    fprintf( fout, '   !   >> generic interface for:\n');
    for j=1:length(intfData(i).ProcIndx)
        fprintf( fout, '   !  %s', repmat(' ',1,tab) );
        writeProcHeader(procData(intfData(i).ProcIndx(j)), fout,tab);
    end
end
for iProc = 1:length(procData)
    if ~isfield(procData(iProc),'Interface') || procData(iProc).Interface < 0 %these are the ones not in a generic interface:
        fprintf( fout, '   !  ');
        writeProcHeader(procData(iProc), fout, tab);
    end
end

end

function [] = writeSummary(fout, procData, typeData, varData, intfData, modData  )
%% create the list
tab=12;
subLen = 80;


stars1 = repmat('*', 1, 40);
dash1  = repmat( '.', 1, subLen);
for iMod = 1:length(modData)
%% write header for this module:
    fprintf( fout, '\n' );
    stars2 = repmat( '*', 1, max(1,132 - 9 - length(stars1) - length( modData(iMod).name)) );
    fprintf( fout, '%s MODULE %s %s\n', stars1, modData(iMod).name, stars2 );
    
    
    for iCom = 1:length(modData(iMod).comments)
        fprintf( fout, '      %s\n' , modData(iMod).comments{iCom} );
    end        
    
%% write out the data defined here:
    if length(varData) > 0 && any([varData(:).module] == iMod)
        
        dash2 = repmat( '.', 1, max(1,subLen - 7 ) );
        fprintf( fout, '\n...data%s\n', dash2 );

        for i=1:length(varData)
            if varData(i).module == iMod
                spaces=repmat(' ',1,80-length(varData(i).name));
                comment = varData(i).comment;
                if ~isempty(comment)
                    comment = ['! ' comment];
                end
                fprintf (fout, '%s%s%s\n', varData(i).name,spaces,comment );
            end
        end
    end

%% write out the procedures defined here:

    if length(procData) > 0 && any([procData(:).module] == iMod)
        
        dash2 = repmat( '.', 1, max(1,subLen - 24 ) );
        fprintf( fout, '\n...subroutines/functions%s\n', dash2 );

        firstProc = true;
        for iProc = 1:length(procData)             
            if procData(iProc).module == iMod          
                if firstProc
                    firstProc = false;
                else
                    fprintf( fout, '%s\n' , dash1);
                end
                writeProcHeader(procData(iProc), fout, tab);

                for iCom = 1:length(procData(iProc).comments)
                    fprintf( fout, '      %s\n' , procData(iProc).comments{iCom} );
                end
            end
        end
    end

end

return
end

function writeProcHeader(thisProc,fout,tab)
    nspaces = tab-length(thisProc.type);
    spaces = repmat(' ',1,nspaces);

    fprintf( fout, '%s%s%s' , thisProc.type, spaces, thisProc.name );
    fprintf( fout, '( ' ) ;
    if ~isempty(thisProc.arguments) 
        fprintf(fout,'%s', thisProc.arguments{1} );
    end
    for iArg = 2:length(thisProc.arguments)
        fprintf(fout,', %s', thisProc.arguments{iArg} );
    end
    fprintf( fout, ' )\n' ); 
    
    return
end

function [intfData, procData] = findInterfaceRoutines( intfData, procData )

% initialize procedures as not being part of a generic interface
procName = cell(length(procData),1);

for i=1:length(procData)
    procData(i).Interface = -1;
    procName{i} = procData(i).name;
end

% mark all procedures inside an interface
for i=1:length(intfData)  
    for iProc = 1:length(intfData(i).ProcName)
        isFound = strcmpi( procName, intfData(i).ProcName{iProc} );
        findx = find(isFound);
        if ~isempty(findx)
            intfData(i).ProcIndx(iProc)     =   findx(1);     
            procData(intfData(i).ProcIndx(iProc)).Interface = i;
        end
    end
end


return
end

