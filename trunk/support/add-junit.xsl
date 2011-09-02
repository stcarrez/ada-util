<?xml version="1.0" encoding="UTF-8"?>
<!--  add-junit.xsl - Add a junit test in an XML junit file
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

    <xsl:param name="classname" select="'Command'"/>
    <xsl:param name="time" select="'1.0'"/>
    <xsl:param name="error" select="''"/>
    <xsl:param name="error_type" select="'Error'"/>

    <xsl:template match="testcase">
      <xsl:param name="name"/>
      <xsl:element name="testcase">
        <xsl:copy-of select="@*"/>
        <xsl:copy-of select="*"/>
      </xsl:element>
    </xsl:template>

    <xsl:template match="/testsuite">
      <xsl:param name="has_error">
        <xsl:choose>
          <xsl:when test="$error != ''">
            <xsl:value-of select="'1'"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="'0'"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:param>
      <xsl:element name="testsuite">        
        <xsl:attribute name="errors">
          <xsl:value-of select="@errors + $has_error"/>
        </xsl:attribute>
        <xsl:attribute name="failures">
          <xsl:value-of select="@failures"/>
        </xsl:attribute>
        <xsl:attribute name="tests">
          <xsl:value-of select="@tests + 1"/>
        </xsl:attribute>
        <xsl:attribute name="name">
          <xsl:value-of select="@name"/>
        </xsl:attribute>
        <xsl:apply-templates>
          <xsl:with-param name="name" select="'SQLite '"/>
        </xsl:apply-templates>

        <!--  Add the new testcase result -->
        <xsl:text>  </xsl:text>
        <xsl:element name="testcase">
          <xsl:attribute name="classname">
            <xsl:value-of select="$classname"/>
          </xsl:attribute>
          <xsl:attribute name="name">
            <xsl:value-of select="$test"/>
          </xsl:attribute>
          <xsl:attribute name="time">
            <xsl:value-of select="$time"/>
          </xsl:attribute>
          <xsl:if test="$has_error = 1">
            <xsl:element name="error">
              <xsl:attribute name="message">
                <xsl:value-of select="$error"/>
              </xsl:attribute>
              <xsl:attribute name="type">
                <xsl:value-of select="$error_type"/>
              </xsl:attribute>
              <xsl:value-of select="$error"/>
            </xsl:element>
          </xsl:if>
        </xsl:element><xsl:text>
</xsl:text>
      </xsl:element>
    </xsl:template>

</xsl:stylesheet>