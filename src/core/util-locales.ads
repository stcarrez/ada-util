-----------------------------------------------------------------------
--  util-locales -- Locale support
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011, 2012, 2013, 2018 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------

with Ada.Containers;

--  The <b>Locales</b> package defines the <b>Locale</b> type to represent
--  the language, country and variant.
--
--  The language is a valid <b>ISO language code</b>.  This is a two-letter
--  lower case code defined by IS-639
--  See http://www.loc.gov/standards/iso639-2/englangn.html
--
--  The country is a valid <b>ISO country code</b>.  These codes are
--  a two letter upper-case code defined by ISO-3166.
--  See http://www.iso.ch/iso/en/prods-services/iso3166ma/02iso-3166-code-lists/list-en1.html
--
--  The variant part is a vendor or browser specific code.
--
--  The <b>Locales</b> package tries to follow the Java <b>Locale</b> class.
package Util.Locales is

   pragma Preelaborate;

   type Locale is private;

   type Locale_Array is array (Positive range <>) of Locale;

   --  List of locales.
   Locales : constant Locale_Array;

   --  Get the lowercase two-letter ISO-639 code.
   function Get_Language (Loc : in Locale) return String;

   --  Get the ISO 639-2 language code.
   function Get_ISO3_Language (Loc : in Locale) return String;

   --  Get the uppercase two-letter ISO-3166 code.
   function Get_Country (Loc : in Locale) return String;

   --  Get the ISO-3166-2 country code.
   function Get_ISO3_Country (Loc : in Locale) return String;

   --  Get the variant code
   function Get_Variant (Loc : in Locale) return String;

   --  Get the locale for the given language code
   function Get_Locale (Language : in String) return Locale;

   --  Get the locale for the given language and country code
   function Get_Locale (Language : in String;
                        Country  : in String) return Locale;

   --  Get the locale for the given language, country and variant code
   function Get_Locale (Language : in String;
                        Country  : in String;
                        Variant  : in String) return Locale;

   --  Get the locale code in the form <i>language</i>_<i>country</i>_<i>variant</i>.
   function To_String (Loc : in Locale) return String;

   --  Compute the hash value of the locale.
   function Hash (Loc : in Locale) return Ada.Containers.Hash_Type;

   NULL_LOCALE : constant Locale;

   --  French language
   FRENCH  : constant Locale;

   --  English language
   ENGLISH : constant Locale;

   --  German language
   GERMAN  : constant Locale;

   --  Italian language
   ITALIAN : constant Locale;

   FRANCE  : constant Locale;
   CANADA  : constant Locale;
   GERMANY : constant Locale;
   ITALY   : constant Locale;
   US      : constant Locale;
   UK      : constant Locale;
private

   type Locale is access constant String;

   ALBANIAN_STR                             : aliased constant String := "sq.sqi";
   ALBANIAN_ALBANIA_STR                     : aliased constant String := "sq_AL.sqi.ALB";
   ARABIC_STR                               : aliased constant String := "ar.ara";
   ARABIC_ALGERIA_STR                       : aliased constant String := "ar_DZ.ara.DZA";
   ARABIC_BAHRAIN_STR                       : aliased constant String := "ar_BH.ara.BHR";
   ARABIC_EGYPT_STR                         : aliased constant String := "ar_EG.ara.EGY";
   ARABIC_IRAQ_STR                          : aliased constant String := "ar_IQ.ara.IRQ";
   ARABIC_JORDAN_STR                        : aliased constant String := "ar_JO.ara.JOR";
   ARABIC_KUWAIT_STR                        : aliased constant String := "ar_KW.ara.KWT";
   ARABIC_LEBANON_STR                       : aliased constant String := "ar_LB.ara.LBN";
   ARABIC_LIBYA_STR                         : aliased constant String := "ar_LY.ara.LBY";
   ARABIC_MOROCCO_STR                       : aliased constant String := "ar_MA.ara.MAR";
   ARABIC_OMAN_STR                          : aliased constant String := "ar_OM.ara.OMN";
   ARABIC_QATAR_STR                         : aliased constant String := "ar_QA.ara.QAT";
   ARABIC_SAUDI_ARABIA_STR                  : aliased constant String := "ar_SA.ara.SAU";
   ARABIC_SUDAN_STR                         : aliased constant String := "ar_SD.ara.SDN";
   ARABIC_SYRIA_STR                         : aliased constant String := "ar_SY.ara.SYR";
   ARABIC_TUNISIA_STR                       : aliased constant String := "ar_TN.ara.TUN";
   ARABIC_UNITED_ARAB_EMIRATES_STR          : aliased constant String := "ar_AE.ara.ARE";
   ARABIC_YEMEN_STR                         : aliased constant String := "ar_YE.ara.YEM";
   BELARUSIAN_STR                           : aliased constant String := "be.bel";
   BELARUSIAN_BELARUS_STR                   : aliased constant String := "be_BY.bel.BLR";
   BULGARIAN_STR                            : aliased constant String := "bg.bul";
   BULGARIAN_BULGARIA_STR                   : aliased constant String := "bg_BG.bul.BGR";
   CATALAN_STR                              : aliased constant String := "ca.cat";
   CATALAN_SPAIN_STR                        : aliased constant String := "ca_ES.cat.ESP";
   CHINESE_STR                              : aliased constant String := "zh.zho";
   CHINESE_CHINA_STR                        : aliased constant String := "zh_CN.zho.CHN";
   CHINESE_HONG_KONG_STR                    : aliased constant String := "zh_HK.zho.HKG";
   CHINESE_SINGAPORE_STR                    : aliased constant String := "zh_SG.zho.SGP";
   CHINESE_TAIWAN_STR                       : aliased constant String := "zh_TW.zho.TWN";
   CROATIAN_STR                             : aliased constant String := "hr.hrv";
   CROATIAN_CROATIA_STR                     : aliased constant String := "hr_HR.hrv.HRV";
   CZECH_STR                                : aliased constant String := "cs.ces";
   CZECH_CZECH_REPUBLIC_STR                 : aliased constant String := "cs_CZ.ces.CZE";
   DANISH_STR                               : aliased constant String := "da.dan";
   DANISH_DENMARK_STR                       : aliased constant String := "da_DK.dan.DNK";
   DUTCH_STR                                : aliased constant String := "nl.nld";
   DUTCH_BELGIUM_STR                        : aliased constant String := "nl_BE.nld.BEL";
   DUTCH_NETHERLANDS_STR                    : aliased constant String := "nl_NL.nld.NLD";
   ENGLISH_STR                              : aliased constant String := "en.eng";
   ENGLISH_AUSTRALIA_STR                    : aliased constant String := "en_AU.eng.AUS";
   ENGLISH_CANADA_STR                       : aliased constant String := "en_CA.eng.CAN";
   ENGLISH_INDIA_STR                        : aliased constant String := "en_IN.eng.IND";
   ENGLISH_IRELAND_STR                      : aliased constant String := "en_IE.eng.IRL";
   ENGLISH_MALTA_STR                        : aliased constant String := "en_MT.eng.MLT";
   ENGLISH_NEW_ZEALAND_STR                  : aliased constant String := "en_NZ.eng.NZL";
   ENGLISH_PHILIPPINES_STR                  : aliased constant String := "en_PH.eng.PHL";
   ENGLISH_SINGAPORE_STR                    : aliased constant String := "en_SG.eng.SGP";
   ENGLISH_SOUTH_AFRICA_STR                 : aliased constant String := "en_ZA.eng.ZAF";
   ENGLISH_UNITED_KINGDOM_STR               : aliased constant String := "en_GB.eng.GBR";
   ENGLISH_UNITED_STATES_STR                : aliased constant String := "en_US.eng.USA";
   ESTONIAN_STR                             : aliased constant String := "et.est";
   ESTONIAN_ESTONIA_STR                     : aliased constant String := "et_EE.est.EST";
   FINNISH_STR                              : aliased constant String := "fi.fin";
   FINNISH_FINLAND_STR                      : aliased constant String := "fi_FI.fin.FIN";
   FRENCH_STR                               : aliased constant String := "fr.fra";
   FRENCH_BELGIUM_STR                       : aliased constant String := "fr_BE.fra.BEL";
   FRENCH_CANADA_STR                        : aliased constant String := "fr_CA.fra.CAN";
   FRENCH_FRANCE_STR                        : aliased constant String := "fr_FR.fra.FRA";
   FRENCH_LUXEMBOURG_STR                    : aliased constant String := "fr_LU.fra.LUX";
   FRENCH_SWITZERLAND_STR                   : aliased constant String := "fr_CH.fra.CHE";
   GERMAN_STR                               : aliased constant String := "de.deu";
   GERMAN_AUSTRIA_STR                       : aliased constant String := "de_AT.deu.AUT";
   GERMAN_GERMANY_STR                       : aliased constant String := "de_DE.deu.DEU";
   GERMAN_LUXEMBOURG_STR                    : aliased constant String := "de_LU.deu.LUX";
   GERMAN_SWITZERLAND_STR                   : aliased constant String := "de_CH.deu.CHE";
   GREEK_STR                                : aliased constant String := "el.ell";
   GREEK_CYPRUS_STR                         : aliased constant String := "el_CY.ell.CYP";
   GREEK_GREECE_STR                         : aliased constant String := "el_GR.ell.GRC";
   HEBREW_STR                               : aliased constant String := "iw.heb";
   HEBREW_ISRAEL_STR                        : aliased constant String := "iw_IL.heb.ISR";
   HINDI_INDIA_STR                          : aliased constant String := "hi_IN.hin.IND";
   HUNGARIAN_STR                            : aliased constant String := "hu.hun";
   HUNGARIAN_HUNGARY_STR                    : aliased constant String := "hu_HU.hun.HUN";
   ICELANDIC_STR                            : aliased constant String := "is.isl";
   ICELANDIC_ICELAND_STR                    : aliased constant String := "is_IS.isl.ISL";
   INDONESIAN_STR                           : aliased constant String := "in.ind";
   INDONESIAN_INDONESIA_STR                 : aliased constant String := "in_ID.ind.IDN";
   IRISH_STR                                : aliased constant String := "ga.gle";
   IRISH_IRELAND_STR                        : aliased constant String := "ga_IE.gle.IRL";
   ITALIAN_STR                              : aliased constant String := "it.ita";
   ITALIAN_ITALY_STR                        : aliased constant String := "it_IT.ita.ITA";
   ITALIAN_SWITZERLAND_STR                  : aliased constant String := "it_CH.ita.CHE";
   JAPANESE_STR                             : aliased constant String := "ja.jpn";
   JAPANESE_JAPAN_STR                       : aliased constant String := "ja_JP.jpn.JPN";
   JAPANESE_JAPAN_JP_STR                    : aliased constant String := "ja_JP_JP.jpn_JP.JPN";
   KOREAN_STR                               : aliased constant String := "ko.kor";
   KOREAN_SOUTH_KOREA_STR                   : aliased constant String := "ko_KR.kor.KOR";
   LATVIAN_STR                              : aliased constant String := "lv.lav";
   LATVIAN_LATVIA_STR                       : aliased constant String := "lv_LV.lav.LVA";
   LITHUANIAN_STR                           : aliased constant String := "lt.lit";
   LITHUANIAN_LITHUANIA_STR                 : aliased constant String := "lt_LT.lit.LTU";
   MACEDONIAN_STR                           : aliased constant String := "mk.mkd";
   MACEDONIAN_MACEDONIA_STR                 : aliased constant String := "mk_MK.mkd.MKD";
   MALAY_STR                                : aliased constant String := "ms.msa";
   MALAY_MALAYSIA_STR                       : aliased constant String := "ms_MY.msa.MYS";
   MALTESE_STR                              : aliased constant String := "mt.mlt";
   MALTESE_MALTA_STR                        : aliased constant String := "mt_MT.mlt.MLT";
   NORWEGIAN_STR                            : aliased constant String := "no.nor";
   NORWEGIAN_NORWAY_STR                     : aliased constant String := "no_NO.nor.NOR";
   NORWEGIAN_NORWAY_NYNORSK_STR             : aliased constant String := "no_NO_NY.nor_NY.NOR";
   POLISH_STR                               : aliased constant String := "pl.pol";
   POLISH_POLAND_STR                        : aliased constant String := "pl_PL.pol.POL";
   PORTUGUESE_STR                           : aliased constant String := "pt.por";
   PORTUGUESE_BRAZIL_STR                    : aliased constant String := "pt_BR.por.BRA";
   PORTUGUESE_PORTUGAL_STR                  : aliased constant String := "pt_PT.por.PRT";
   ROMANIAN_STR                             : aliased constant String := "ro.ron";
   ROMANIAN_ROMANIA_STR                     : aliased constant String := "ro_RO.ron.ROU";
   RUSSIAN_STR                              : aliased constant String := "ru.rus";
   RUSSIAN_RUSSIA_STR                       : aliased constant String := "ru_RU.rus.RUS";
   SERBIAN_STR                              : aliased constant String := "sr.srp";
   SERBIAN_BOSNIA_AND_HERZEGOVINA_STR       : aliased constant String := "sr_BA.srp.BIH";
   SERBIAN_MONTENEGRO_STR                   : aliased constant String := "sr_ME.srp.MNE";
   SERBIAN_SERBIA_STR                       : aliased constant String := "sr_RS.srp.SRB";
   SERBIAN_SERBIA_AND_MONTENEGRO_STR        : aliased constant String := "sr_CS.srp.SCG";
   SLOVAK_STR                               : aliased constant String := "sk.slk";
   SLOVAK_SLOVAKIA_STR                      : aliased constant String := "sk_SK.slk.SVK";
   SLOVENIAN_STR                            : aliased constant String := "sl.slv";
   SLOVENIAN_SLOVENIA_STR                   : aliased constant String := "sl_SI.slv.SVN";
   SPANISH_STR                              : aliased constant String := "es.spa";
   SPANISH_ARGENTINA_STR                    : aliased constant String := "es_AR.spa.ARG";
   SPANISH_BOLIVIA_STR                      : aliased constant String := "es_BO.spa.BOL";
   SPANISH_CHILE_STR                        : aliased constant String := "es_CL.spa.CHL";
   SPANISH_COLOMBIA_STR                     : aliased constant String := "es_CO.spa.COL";
   SPANISH_COSTA_RICA_STR                   : aliased constant String := "es_CR.spa.CRI";
   SPANISH_DOMINICAN_REPUBLIC_STR           : aliased constant String := "es_DO.spa.DOM";
   SPANISH_ECUADOR_STR                      : aliased constant String := "es_EC.spa.ECU";
   SPANISH_EL_SALVADOR_STR                  : aliased constant String := "es_SV.spa.SLV";
   SPANISH_GUATEMALA_STR                    : aliased constant String := "es_GT.spa.GTM";
   SPANISH_HONDURAS_STR                     : aliased constant String := "es_HN.spa.HND";
   SPANISH_MEXICO_STR                       : aliased constant String := "es_MX.spa.MEX";
   SPANISH_NICARAGUA_STR                    : aliased constant String := "es_NI.spa.NIC";
   SPANISH_PANAMA_STR                       : aliased constant String := "es_PA.spa.PAN";
   SPANISH_PARAGUAY_STR                     : aliased constant String := "es_PY.spa.PRY";
   SPANISH_PERU_STR                         : aliased constant String := "es_PE.spa.PER";
   SPANISH_PUERTO_RICO_STR                  : aliased constant String := "es_PR.spa.PRI";
   SPANISH_SPAIN_STR                        : aliased constant String := "es_ES.spa.ESP";
   SPANISH_UNITED_STATES_STR                : aliased constant String := "es_US.spa.USA";
   SPANISH_URUGUAY_STR                      : aliased constant String := "es_UY.spa.URY";
   SPANISH_VENEZUELA_STR                    : aliased constant String := "es_VE.spa.VEN";
   SWEDISH_STR                              : aliased constant String := "sv.swe";
   SWEDISH_SWEDEN_STR                       : aliased constant String := "sv_SE.swe.SWE";
   THAI_STR                                 : aliased constant String := "th.tha";
   THAI_THAILAND_STR                        : aliased constant String := "th_TH.tha.THA";
   THAI_THAILAND_TH_STR                     : aliased constant String := "th_TH_TH.tha_TH.THA";
   TURKISH_STR                              : aliased constant String := "tr.tur";
   TURKISH_TURKEY_STR                       : aliased constant String := "tr_TR.tur.TUR";
   UKRAINIAN_STR                            : aliased constant String := "uk.ukr";
   UKRAINIAN_UKRAINE_STR                    : aliased constant String := "uk_UA.ukr.UKR";
   VIETNAMESE_STR                           : aliased constant String := "vi.vie";
   VIETNAMESE_VIETNAM_STR                   : aliased constant String := "vi_VN.vie.VNM";

   --  The locales must be sorted on the locale string.
   Locales : constant Locale_Array (1 .. 152) := (
                                                  ARABIC_STR'Access,
                                                  ARABIC_UNITED_ARAB_EMIRATES_STR'Access,
                                                  ARABIC_BAHRAIN_STR'Access,
                                                  ARABIC_ALGERIA_STR'Access,
                                                  ARABIC_EGYPT_STR'Access,
                                                  ARABIC_IRAQ_STR'Access,
                                                  ARABIC_JORDAN_STR'Access,
                                                  ARABIC_KUWAIT_STR'Access,
                                                  ARABIC_LEBANON_STR'Access,
                                                  ARABIC_LIBYA_STR'Access,
                                                  ARABIC_MOROCCO_STR'Access,
                                                  ARABIC_OMAN_STR'Access,
                                                  ARABIC_QATAR_STR'Access,
                                                  ARABIC_SAUDI_ARABIA_STR'Access,
                                                  ARABIC_SUDAN_STR'Access,
                                                  ARABIC_SYRIA_STR'Access,
                                                  ARABIC_TUNISIA_STR'Access,
                                                  ARABIC_YEMEN_STR'Access,
                                                  BELARUSIAN_STR'Access,
                                                  BELARUSIAN_BELARUS_STR'Access,
                                                  BULGARIAN_STR'Access,
                                                  BULGARIAN_BULGARIA_STR'Access,
                                                  CATALAN_STR'Access,
                                                  CATALAN_SPAIN_STR'Access,
                                                  CZECH_STR'Access,
                                                  CZECH_CZECH_REPUBLIC_STR'Access,
                                                  DANISH_STR'Access,
                                                  DANISH_DENMARK_STR'Access,
                                                  GERMAN_STR'Access,
                                                  GERMAN_AUSTRIA_STR'Access,
                                                  GERMAN_SWITZERLAND_STR'Access,
                                                  GERMAN_GERMANY_STR'Access,
                                                  GERMAN_LUXEMBOURG_STR'Access,
                                                  GREEK_STR'Access,
                                                  GREEK_CYPRUS_STR'Access,
                                                  GREEK_GREECE_STR'Access,
                                                  ENGLISH_STR'Access,
                                                  ENGLISH_AUSTRALIA_STR'Access,
                                                  ENGLISH_CANADA_STR'Access,
                                                  ENGLISH_UNITED_KINGDOM_STR'Access,
                                                  ENGLISH_IRELAND_STR'Access,
                                                  ENGLISH_INDIA_STR'Access,
                                                  ENGLISH_MALTA_STR'Access,
                                                  ENGLISH_NEW_ZEALAND_STR'Access,
                                                  ENGLISH_PHILIPPINES_STR'Access,
                                                  ENGLISH_SINGAPORE_STR'Access,
                                                  ENGLISH_UNITED_STATES_STR'Access,
                                                  ENGLISH_SOUTH_AFRICA_STR'Access,
                                                  SPANISH_STR'Access,
                                                  SPANISH_ARGENTINA_STR'Access,
                                                  SPANISH_BOLIVIA_STR'Access,
                                                  SPANISH_CHILE_STR'Access,
                                                  SPANISH_COLOMBIA_STR'Access,
                                                  SPANISH_COSTA_RICA_STR'Access,
                                                  SPANISH_DOMINICAN_REPUBLIC_STR'Access,
                                                  SPANISH_ECUADOR_STR'Access,
                                                  SPANISH_SPAIN_STR'Access,
                                                  SPANISH_GUATEMALA_STR'Access,
                                                  SPANISH_HONDURAS_STR'Access,
                                                  SPANISH_MEXICO_STR'Access,
                                                  SPANISH_NICARAGUA_STR'Access,
                                                  SPANISH_PANAMA_STR'Access,
                                                  SPANISH_PERU_STR'Access,
                                                  SPANISH_PUERTO_RICO_STR'Access,
                                                  SPANISH_PARAGUAY_STR'Access,
                                                  SPANISH_EL_SALVADOR_STR'Access,
                                                  SPANISH_UNITED_STATES_STR'Access,
                                                  SPANISH_URUGUAY_STR'Access,
                                                  SPANISH_VENEZUELA_STR'Access,
                                                  ESTONIAN_STR'Access,
                                                  ESTONIAN_ESTONIA_STR'Access,
                                                  FINNISH_STR'Access,
                                                  FINNISH_FINLAND_STR'Access,
                                                  FRENCH_STR'Access,
                                                  FRENCH_BELGIUM_STR'Access,
                                                  FRENCH_CANADA_STR'Access,
                                                  FRENCH_SWITZERLAND_STR'Access,
                                                  FRENCH_FRANCE_STR'Access,
                                                  FRENCH_LUXEMBOURG_STR'Access,
                                                  IRISH_STR'Access,
                                                  IRISH_IRELAND_STR'Access,
                                                  HINDI_INDIA_STR'Access,
                                                  CROATIAN_STR'Access,
                                                  CROATIAN_CROATIA_STR'Access,
                                                  HUNGARIAN_STR'Access,
                                                  HUNGARIAN_HUNGARY_STR'Access,
                                                  INDONESIAN_STR'Access,
                                                  INDONESIAN_INDONESIA_STR'Access,
                                                  ICELANDIC_STR'Access,
                                                  ICELANDIC_ICELAND_STR'Access,
                                                  ITALIAN_STR'Access,
                                                  ITALIAN_SWITZERLAND_STR'Access,
                                                  ITALIAN_ITALY_STR'Access,
                                                  HEBREW_STR'Access,
                                                  HEBREW_ISRAEL_STR'Access,
                                                  JAPANESE_STR'Access,
                                                  JAPANESE_JAPAN_STR'Access,
                                                  JAPANESE_JAPAN_JP_STR'Access,
                                                  KOREAN_STR'Access,
                                                  KOREAN_SOUTH_KOREA_STR'Access,
                                                  LITHUANIAN_STR'Access,
                                                  LITHUANIAN_LITHUANIA_STR'Access,
                                                  LATVIAN_STR'Access,
                                                  LATVIAN_LATVIA_STR'Access,
                                                  MACEDONIAN_STR'Access,
                                                  MACEDONIAN_MACEDONIA_STR'Access,
                                                  MALAY_STR'Access,
                                                  MALAY_MALAYSIA_STR'Access,
                                                  MALTESE_STR'Access,
                                                  MALTESE_MALTA_STR'Access,
                                                  DUTCH_STR'Access,
                                                  DUTCH_BELGIUM_STR'Access,
                                                  DUTCH_NETHERLANDS_STR'Access,
                                                  NORWEGIAN_STR'Access,
                                                  NORWEGIAN_NORWAY_STR'Access,
                                                  NORWEGIAN_NORWAY_NYNORSK_STR'Access,
                                                  POLISH_STR'Access,
                                                  POLISH_POLAND_STR'Access,
                                                  PORTUGUESE_STR'Access,
                                                  PORTUGUESE_BRAZIL_STR'Access,
                                                  PORTUGUESE_PORTUGAL_STR'Access,
                                                  ROMANIAN_STR'Access,
                                                  ROMANIAN_ROMANIA_STR'Access,
                                                  RUSSIAN_STR'Access,
                                                  RUSSIAN_RUSSIA_STR'Access,
                                                  SLOVAK_STR'Access,
                                                  SLOVAK_SLOVAKIA_STR'Access,
                                                  SLOVENIAN_STR'Access,
                                                  SLOVENIAN_SLOVENIA_STR'Access,
                                                  ALBANIAN_STR'Access,
                                                  ALBANIAN_ALBANIA_STR'Access,
                                                  SERBIAN_STR'Access,
                                                  SERBIAN_BOSNIA_AND_HERZEGOVINA_STR'Access,
                                                  SERBIAN_SERBIA_AND_MONTENEGRO_STR'Access,
                                                  SERBIAN_MONTENEGRO_STR'Access,
                                                  SERBIAN_SERBIA_STR'Access,
                                                  SWEDISH_STR'Access,
                                                  SWEDISH_SWEDEN_STR'Access,
                                                  THAI_STR'Access,
                                                  THAI_THAILAND_STR'Access,
                                                  THAI_THAILAND_TH_STR'Access,
                                                  TURKISH_STR'Access,
                                                  TURKISH_TURKEY_STR'Access,
                                                  UKRAINIAN_STR'Access,
                                                  UKRAINIAN_UKRAINE_STR'Access,
                                                  VIETNAMESE_STR'Access,
                                                  VIETNAMESE_VIETNAM_STR'Access,
                                                  CHINESE_STR'Access,
                                                  CHINESE_CHINA_STR'Access,
                                                  CHINESE_HONG_KONG_STR'Access,
                                                  CHINESE_SINGAPORE_STR'Access,
                                                  CHINESE_TAIWAN_STR'Access);

   NULL_LOCALE : constant Locale := null;

   FRENCH  : constant Locale := FRENCH_STR'Access;

   ITALIAN : constant Locale := ITALIAN_STR'Access;

   GERMAN  : constant Locale := GERMAN_STR'Access;

   ENGLISH : constant Locale := ENGLISH_STR'Access;

   FRANCE  : constant Locale := FRENCH_FRANCE_STR'Access;

   ITALY   : constant Locale := ITALIAN_ITALY_STR'Access;

   CANADA  : constant Locale := ENGLISH_CANADA_STR'Access;

   GERMANY : constant Locale := GERMAN_GERMANY_STR'Access;

   US      : constant Locale := ENGLISH_UNITED_STATES_STR'Access;

   UK      : constant Locale := ENGLISH_UNITED_KINGDOM_STR'Access;

end Util.Locales;
