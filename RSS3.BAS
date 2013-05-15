'RSS parser v3 by Andrew D'Angelo

'Can use freshly-downloaded feeds - no line-breaking needed!
'Uses updated XML.BAS libraries
'I wrote the framework in about 3 hours.
'Making it look nice might take a bit longer.

'Version history:
'v1 - Used select-case, sequential proccessing to parse tags
        'Could parse larger files, but very low compatability rate
'v2 - Integrated a rough version of XML.BAS.
        'Speed improvements, needed line broken files. Less space available
'v3 - Much faster, uses XML.BAS v2. Size cut from 42k to 11k thru XML.BAS v2
        'can use files with line breaks.
        'XML.BAS v2 improvements:
                'Better handling/proccessing for existing functions
                'Added enumEle (Count number of items)
                'Added remEle (Remove element from string)

'To do:
        'Make interface look nicer!
        'Add the help!
        'Get rid of the tabs/extra breaks when not de-line-broken

'= = = = = X M L   P A R S I N G   D E C L A R A T I O N S
DECLARE FUNCTION getTagAttribute$ (rawData AS STRING, element AS STRING, attribute AS STRING)
DECLARE FUNCTION getXMLVersion! (rawData AS STRING)
DECLARE FUNCTION getRootElement$ (rawData AS STRING)
DECLARE FUNCTION getElementContent$ (rawData AS STRING, element AS STRING)
DECLARE FUNCTION remEle$ (rawData AS STRING, eleSpec AS STRING)
DECLARE FUNCTION enumEle! (rawData AS STRING, eleSpec AS STRING)
DECLARE FUNCTION remHTMLSym$ (rawData AS STRING)
'= = = = = = = = = = = = = = = = = = = = = = = = =

'= = = = = T E X T O O L S   D E C L A R A T I O N S
DECLARE FUNCTION center! (inputStr AS STRING)
        'Usage: LOCATE [ROW], center([TARGET]); PRINT [TARGET]
DECLARE FUNCTION findReplace$ (searchInside AS STRING, target AS STRING, replaceWith AS STRING)
DECLARE SUB synthStatusBar ()
DECLARE SUB writeStatusBar (message$)
DECLARE FUNCTION remHTMLSym$ (rawData AS STRING)
'= = = = = = = = = = = = = = = = = = = = = = = = =

ON ERROR GOTO errorHandling

IF COMMAND$ = "" THEN
PRINT "RSS parser v3"
PRINT "<<Display help>>"
END
END IF

OPEN COMMAND$ FOR INPUT AS #1
DO
        LINE INPUT #1, buffer$
        buffer$ = findReplace$(buffer$, CHR$(10), "")
        buffer$ = findReplace$(buffer$, CHR$(9), "")
        'buffer$ = findReplace$(buffer$, "  ", "")

        rawData$ = rawData$ + buffer$
LOOP UNTIL EOF(1)

CLS

feedTitle$ = getElementContent$(rawData$, "title")
rawData$ = remEle(rawData$, "title")
PRINT "Feed title: " + feedTitle$

feedLink$ = getElementContent$(rawData$, "link")
rawData$ = remEle(rawData$, "link")
PRINT "Feed link: " + feedLink$

feedDesc$ = getElementContent$(rawData$, "description")
rawData$ = remEle(rawData$, "description")
PRINT "Feed description: " + feedDesc$

feedLang$ = getElementContent$(rawData$, "language")
rawData$ = remEle(rawData$, "language")
PRINT "Feed language: " + feedLang$

feedCopy$ = getElementContent$(rawData$, "copyright")
rawData$ = remEle(rawData$, "copyright")
PRINT "Feed copyright: " + feedCopy$

feedLBD$ = getElementContent$(rawData$, "lastBuildDate")
rawData$ = remEle(rawData$, "lastBuildDate")
PRINT "Feed updated: " + feedLBD$

numItems = enumEle(rawData$, "item")
DIM titles(numItems) AS STRING
DIM links(numItems)  AS STRING
DIM descs(numItems)  AS STRING
DIM authors(numItems) AS STRING
DIM pubDates(numItems) AS STRING
DIM guids(numItems)   AS STRING

SHELL "pause"

FOR i = 1 TO numItems
        titles(i) = getElementContent$(rawData$, "title")
        links(i) = getElementContent$(rawData$, "link")
        descs(i) = getElementContent$(rawData$, "description")
        authors(i) = getElementContent$(rawData$, "author")
        pubDates(i) = getElementContent$(rawData$, "pubDate")
        guids(i) = getElementContent$(rawData$, "guid")

        titles(i) = findReplace$(titles(i), "<![CDATA[", "")
        titles(i) = findReplace$(titles(i), "]]>", "")
        titles(i) = findReplace$(titles(i), "&#8217;", CHR$(39))
        titles(i) = findReplace$(titles(i), "&#8220;", CHR$(34))
        titles(i) = findReplace$(titles(i), CHR$(228), CHR$(132))
        titles(i) = findReplace$(titles(i), CHR$(246), CHR$(148))
        titles(i) = findReplace$(titles(i), CHR$(252), CHR$(129))
        titles(i) = findReplace$(titles(i), CHR$(223), CHR$(225))
        titles(i) = findReplace$(titles(i), "&nbsp;", "")
        titles(i) = findReplace$(titles(i), "&lt;", "<")
        titles(i) = findReplace$(titles(i), "&gt;", ">")
        titles(i) = findReplace$(titles(i), "&quot;", CHR$(34))
        titles(i) = findReplace$(titles(i), "&#39;", CHR$(39))

        descs(i) = findReplace$(descs(i), "<![CDATA[", "")
        descs(i) = findReplace$(descs(i), "]]>", "")
        descs(i) = findReplace$(descs(i), "&#8217;", CHR$(39))
        descs(i) = findReplace$(descs(i), "&#8220;", CHR$(34))
        descs(i) = findReplace$(descs(i), "&#8221;", CHR$(34))
        descs(i) = findReplace$(descs(i), CHR$(228), CHR$(132))
        descs(i) = findReplace$(descs(i), CHR$(246), CHR$(148))
        descs(i) = findReplace$(descs(i), CHR$(252), CHR$(129))
        descs(i) = findReplace$(descs(i), CHR$(223), CHR$(225))
        descs(i) = findReplace$(descs(i), "&nbsp;", "")
        descs(i) = findReplace$(descs(i), "&lt;", "<")
        descs(i) = findReplace$(descs(i), "&gt;", ">")
        descs(i) = findReplace$(descs(i), "&quot;", CHR$(34))
        descs(i) = findReplace$(descs(i), "&#39;", CHR$(39))


        rawData$ = remEle(rawData$, "item")

        CLS

        COLOR 15, 0: PRINT "Displaying item";
        COLOR 7, 0: PRINT STR$(i)
        PRINT
        COLOR 15, 0: PRINT "Title: ";
        COLOR 7, 0: PRINT titles(i)
        COLOR 15, 0: PRINT "Link: ";
        COLOR 7, 0: PRINT links(i)
        COLOR 15, 0: PRINT "Description: ";
        COLOR 7, 0: PRINT descs(i)
        COLOR 15, 0: PRINT "Author: ";
        COLOR 7, 0: PRINT authors(i)
        COLOR 15, 0: PRINT "Date: ";
        COLOR 7, 0: PRINT pubDates(i)
        COLOR 15, 0: PRINT "GUID: ";
        COLOR 7, 0: PRINT guids(i)
        PRINT
        SHELL "pause"

NEXT i

END

errorHandling:
SELECT CASE ERR
CASE 14
        PRINT "XML file too large, exiting..."
        END
CASE 53
        PRINT "RSS parser v3 cannot find ths file!"
        END
CASE ELSE
        PRINT "Error " + STR$(ERR) + ", exiting..."
        END
END SELECT

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

FUNCTION findReplace$ (searchInside AS STRING, target AS STRING, replaceWith AS STRING)
        failSafe = 0
        DO
                IF INSTR(searchInside$, target$) = 0 THEN : EXIT DO
                leftPart$ = LEFT$(searchInside$, INSTR(searchInside$, target$) - 1)
                rightPart$ = RIGHT$(searchInside$, LEN(searchInside$) - LEN(leftPart$) - LEN(target$))
                searchInside$ = leftPart$ + replaceWith$ + rightPart$
                failSafe = failSafe + 1
        LOOP UNTIL INSTR(searchInside$, target$) = 0 OR failSafe = 512
        IF failSafe = 2048 THEN
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
                temp$ = MID$(rightOfBeginTagBegin$, eleBeginTagEndPos, eleContentLen)
                temp$ = findReplace$(temp$, CHR$(10), "")
                getElementContent$ = findReplace$(temp$, CHR$(9), "")
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

FUNCTION remEle$ (rawData AS STRING, eleSpec AS STRING)
        eleOpenTagBegin = INSTR(rawData$, "<" + eleSpec$) - 1
        IF eleOpenTagBegin <= 0 THEN
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

