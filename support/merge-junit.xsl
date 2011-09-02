<?xml version="1.0" encoding="UTF-8"?>
<!--  merge-junit.xsl - Merge several XML junit files (caveats: file names are hard coded (:- )
-  Copyright (C) 2011 Stephane Carrez
-  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
-
-  Licensed under the Apache License, Version 2.0 (the "License");
-  you may not use this file except in compliance with the License.
-  You may obtain a copy of the License at
-
-      http://www.apache.org/licenses/LICENSE-2.0
-
-  Unless required by applicable law or agreed to in writing, software
-  distributed under the License is distributed on an "AS IS" BASIS,
-  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-  See the License for the specific language governing permissions and
-  limitations under the License.
-->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="xml" indent="yes"/>

    <xsl:template match="testcase">
      <xsl:param name="name"/>
      <xsl:element name="testcase">
        <xsl:copy-of select="@*"/>
        <xsl:attribute name="classname"><xsl:value-of select="$name"/><xsl:value-of select="@classname"/>
        </xsl:attribute>
        <xsl:copy-of select="*"/>
      </xsl:element>
    </xsl:template>

    <xsl:template match="/testsuite">
      <xsl:element name="testsuite">        
        <xsl:attribute name="errors">
          <xsl:value-of select="@errors + document('ado-mysql-junit.xml')/testsuite/@errors"/>
        </xsl:attribute>
        <xsl:attribute name="failures">
          <xsl:value-of select="@failures + document('ado-mysql-junit.xml')/testsuite/@failures"/>
        </xsl:attribute>
        <xsl:attribute name="tests">
          <xsl:value-of select="@tests + document('ado-mysql-junit.xml')/testsuite/@tests"/>
        </xsl:attribute>
        <xsl:attribute name="name">
          <xsl:value-of select="@name"/>
        </xsl:attribute>
        <xsl:apply-templates>
          <xsl:with-param name="name" select="'SQLite '"/>
        </xsl:apply-templates>
        <xsl:for-each select="document('ado-mysql-junit.xml')/testsuite">
          <xsl:apply-templates>
            <xsl:with-param name="name" select="'MySQL '"/>
          </xsl:apply-templates>     
        </xsl:for-each>
      </xsl:element>
    </xsl:template>

</xsl:stylesheet>