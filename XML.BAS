'QuickBasic XML parsing functions, JSON parsing/creation, TexTools
'2011 Andrew D'Angelo
'mailto:excel@pharcyde.org

'Requires XML/JSON files with line breaks removed
'Installation: File -> Merge... -> XML.BAS

'To do:
'JSON support is a work in progress

DECLARE FUNCTION fileToString$ (filePath AS STRING)

'= = = = = X M L   P A R S I N G   D E C L A R A T I O N S
DECLARE FUNCTION getTagAttribute$ (rawData AS STRING, element AS STRING, attribute AS STRING)
DECLARE FUNCTION getXMLVersion! (rawData AS STRING)
DECLARE FUNCTION getRootElement$ (rawData AS STRING)
DECLARE FUNCTION getElementContent$ (rawData AS STRING, element AS STRING)
DECLARE FUNCTION remEle$ (rawData AS STRING, eleSpec AS STRING)
DECLARE FUNCTION enumEle! (rawData AS STRING, eleSpec AS STRING)
DECLARE FUNCTION remHTMLSym$ (rawData AS STRING)
'= = = = = = = = = = = = = = = = = = = = = = = = =

'= = = = = J S O N   P A R S I N G   D E C L A R A T I O N S
DECLARE FUNCTION jsonGetEleContent$ (rawData$, target$)
'= = = = = = = = = = = = = = = = = = = = = = = = =       

'= = = = = T E X T O O L S   D E C L A R A T I O N S
DECLARE FUNCTION center! (inputStr AS STRING)
        'Usage: LOCATE [ROW], center([TARGET]); PRINT [TARGET]
DECLARE FUNCTION findReplace$ (searchInside AS STRING, target AS STRING, replaceWith AS STRING)
DECLARE SUB synthStatusBar ()
DECLARE SUB writeStatusBar (message$)
DECLARE FUNCTION remHTMLSym$ (rawData AS STRING)
'= = = = = = = = = = = = = = = = = = = = = = = = =

FUNCTION center (inputStr AS STRING)
        'Std screen has 80 columns
        center = CINT((80 - LEN(inputStr$)) / 2)
END FUNCTION

FUNCTION enumEle! (rawData AS STRING, eleSpec AS STRING)
        counter = 0

        temp$ = rawData$

        IF INSTR(temp$, "<" + eleSpec$ + ">") = 0 THEN
                'PRINT "Spec not found"
                GOTO breakEnumEle
        END IF

        DO
                IF INSTR(temp$, "<" + eleSpec$) = 0 OR counter = 512 THEN
                        GOTO breakEnumEle
                END IF

                counter = counter + 1
                temp$ = remEle$(temp$, eleSpec$)
                'PRINT temp$
        LOOP

breakEnumEle:
        enumEle! = counter

END FUNCTION

FUNCTION fileToString$ (filePath AS STRING)

'check exists
'load into string with line input

'Optimization:
'Get rid of need to de-line-break
'Remove string size limit (chunks?)

END FUNCTION

FUNCTION findReplace$ (searchInside AS STRING, target AS STRING, replaceWith AS STRING)
        failSafe = 0
        DO
                IF INSTR(searchInside$, target$) = 0 THEN : EXIT DO
                leftPart$ = LEFT$(searchInside$, INSTR(searchInside$, target$) - 1)
                rightPart$ = RIGHT$(searchInside$, LEN(searchInside$) - LEN(leftPart$) - LEN(target$))
                searchInside$ = leftPart$ + replaceWith$ + rightPart$
                failSafe = failSafe + 1
        LOOP UNTIL INSTR(searchInside$, target$) = 0 OR failSafe = 512
        IF failSafe = 512 THEN
                COLOR 12, 0
                PRINT "Find & replace exited forcibly!"
                COLOR 15, 0
                PRINT "Result follows: ";
                COLOR 7, 0
        END IF
        findReplace$ = searchInside$
END FUNCTION

FUNCTION getElementContent$ (rawData AS STRING, element AS STRING)
        eleBeginTagBeginPos = INSTR(rawData$, element$)
        rightOfBeginTagBegin$ = RIGHT$(rawData$, LEN(rawData$) - eleBeginTagBeginPos)
        eleBeginTagEndPos = INSTR(rightOfBeginTagBegin$, ">") + 1
        eleEndTagBeginPos = INSTR(rightOfBeginTagBegin$, "</" + element$)
        eleContentLen = eleEndTagBeginPos - eleBeginTagEndPos

        IF eleBeginTagBeginPos * eleEndTagBeginPos * eleEndTagBeginPos * eleContentLen < 1 THEN
                getElementContent$ = "<<no data>>"
        ELSE
                getElementContent$ = MID$(rightOfBeginTagBegin$, eleBeginTagEndPos, eleContentLen)
        END IF

END FUNCTION

FUNCTION getRootElement$ (rawData AS STRING)
        rightOfHeader$ = RIGHT$(rawData$, LEN(rawData$) - INSTR(rawData$, ">"))
        rootEleBeginPos = INSTR(rightOfHeader$, "<") + 1
        rootEleEndPos = INSTR(rightOfHeader$, ">")
        rootEleLen = rootEleEndPos - rootEleBeginPos

        IF INSTR(rightOfHeader$, "?>") <> 0 THEN
                DO
                rightOfHeader$ = RIGHT$(rightOfHeader$, LEN(rawRightOfHeader$) - INSTR(rightOfHeader$, ">"))
                rootEleBeginPos = INSTR(rightOfHeader$, "<") + 1
                rootEleEndPos = INSTR(rightOfHeader$, ">")
                rootEleLen = rootEleEndPos - rootEleBeginPos
                LOOP WHILE INSTR(rightOfHeader$, "?>") <> 0
        END IF

        'PRINT rootEleBeginPos
        'PRINT rootEleEndPos
        'PRINT rootEleLen
        'SHELL "pause"
        IF rootEleBeginPos * rootEleEndPos * rootEleLen = 0 THEN
                getRootElement$ = "<<no data>>"
        ELSE
                getRootElement$ = MID$(rightOfHeader$, rootEleBeginPos, rootEleLen)
        END IF
END FUNCTION

FUNCTION getTagAttribute$ (rawData AS STRING, element AS STRING, attribute AS STRING)
        rawElemBeginPos = INSTR(rawData$, "<" + element$)
        rawElemEndPos = INSTR(RIGHT$(rawData$, LEN(rawData$) - rawElemBeginPos), ">") + rawElemBeginPos
        rawElemLen = rawElemEndPos - rawElemBeginPos
        rawElem$ = MID$(rawData, rawElemBeginPos, rawElemLen)
       
        attBeginPos = INSTR(rawElem$, attribute$ + "=") + LEN(attribute$) + 2
        attEndPos = INSTR(RIGHT$(rawElem$, LEN(rawElem$) - attBeginPos), CHR$(34)) + attBeginPos
        attLen = attEndPos - attBeginPos
      
        IF MID$(rawElem$, attBeginPos, attLen) = "" THEN
                getTagAttribute$ = "<<not specified>>"
        ELSE
                getTagAttribute$ = MID$(rawElem$, attBeginPos, attLen)
        END IF

END FUNCTION

FUNCTION getXMLVersion (rawData AS STRING)
        rawHeaderEndPos = INSTR(rawData$, "?>") + 2
        rawHeader$ = MID$(rawData$, 1, rawHeaderEndPos)
        versionBeginPos = INSTR(rawHeader$, "version=") + 9
        versionEndPos = INSTR(RIGHT$(rawHeader$, LEN(rawHeader$) - versionBeginPos), CHR$(34)) + versionBeginPos
        versionLen = versionEndPos - versionBeginPos
        IF LEFT$(rawData$, 2) <> "<?" THEN
                getXMLVersion = 0
        ELSE
                getXMLVersion = VAL(MID$(rawHeader$, versionBeginPos, versionLen))
        END IF
END FUNCTION

FUNCTION jsonGetEleContent$ (rawData$, target$)

        tagBeginPos = instr$(rawData$, target$) - 1
        cutOut$ = RIGHT$(rawData$, LEN(rawData$) - tagBeginPos)
        eleEndPos = instr$(cutOut$, ",") - 1
        tagEndPos = instr$(cutOut$, ":") + 1
        IF MID$(cutOut$, tagEndPos + 1, 1) = " " THEN : eleBeginPos = tagEndPos + 1

        rightOfTag$ = RIGHT$(cutOut$, LEN(coutOut$) - tagEndPos)

END FUNCTION

FUNCTION remEle$ (rawData AS STRING, eleSpec AS STRING)
        eleOpenTagBegin = INSTR(rawData$, "<" + eleSpec$) - 1
        IF eleOpenTagBegin = 0 THEN
                eleOpenTagBegin = 1
                itemstarts = 1
        END IF

        IF eleOpenTagBegin = 0 THEN 'Not found
                'PRINT "remEle ERROR"
                remEle$ = rawData$
                GOTO breakRemEle
        END IF

        'eleOpenTagEnd = eleOpenTagBegin + 2 + LEN(eleSpec$)
        IF itemstarts = 0 THEN
                beforeEle$ = LEFT$(rawData$, eleOpenTagBegin)
        END IF
        eleOpenTagOnward$ = RIGHT$(rawData$, LEN(rawData$) - eleOpenTagBegin)
        'PRINT "eOTO " + eleOpenTagOnwards$

        eleEndTagEnd = INSTR(eleOpenTagOnward$, "/" + eleSpec$) + 1 + LEN(eleSpec$)

        afterEle$ = RIGHT$(eleOpenTagOnward$, LEN(eleOpenTagOnward$) - eleEndTagEnd)

        'PRINT "bE " + beforeEle$
        'PRINT "aE " + afterEle$

        remEle$ = beforeEle$ + afterEle$


breakRemEle:
END FUNCTION

FUNCTION remHTMLSym$ (rawData AS STRING)
        rawData$ = findReplace$(rawData$, "&amp;", "&")
        rawData$ = findReplace$(rawData$, "&#39;", CHR$(39))
        rawData$ = findReplace$(rawData$, "&#039;", CHR$(39))
        rawData$ = findReplace$(rawData$, "&apos;", CHR$(39))
        rawData$ = findReplace$(rawData$, "&quot;", CHR$(34))
        rawData$ = findReplace$(rawData$, "&gt;", ">")
        rawData$ = findReplace$(rawData$, "&lt;", "<")
        rawData$ = findReplace$(rawData$, "&nbsp;", " ")

        HTMLSym$ = rawData$
END FUNCTION

SUB synthStatusBar
        ' Set up prompt on bottom line of screen and turn cursor on:
        LOCATE 24, 1, 1
        COLOR 15, 0
        PRINT STRING$(80, "_");
        COLOR 7, 0

        VIEW PRINT 1 TO 23              ' Print between lines 1 & 23.
END SUB

SUB writeStatusBar (message$)
        FOR i = 1 TO 80
                LOCATE 25, i
                PRINT CHR$(32)
        NEXT i
        LOCATE 25, center(message$)
        PRINT message$
END SUB

