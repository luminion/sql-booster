package io.github.luminion.sqlbooster.util;

public abstract class StrConvertUtils {

    public static String underscoreToCamelCase(String str) {
        StringBuilder sb = new StringBuilder();
        boolean upperCase = false;
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (c == '_') {
                upperCase = true;
            } else if (upperCase) {
                sb.append(Character.toUpperCase(c));
                upperCase = false;
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    public static String camelCaseToUnderscore(String str) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (Character.isUpperCase(c)) {
                // 连续大写缩写（如 XML、HTTP）只在缩写结束前插入下划线，避免 XMLParser → x_m_l_parser。
                // 规则：前一字符非大写，或后一字符是小写（说明当前是缩写最后一位，下一段是新单词）。
                boolean prevNotUpper = i > 0 && !Character.isUpperCase(str.charAt(i - 1));
                boolean nextIsLower = i + 1 < str.length() && Character.isLowerCase(str.charAt(i + 1));
                if (i > 0 && (prevNotUpper || nextIsLower)) {
                    sb.append('_');
                }
                sb.append(Character.toLowerCase(c));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    public static String pascalCaseToUnderscore(String str) {
        String s = camelCaseToUnderscore(str);
        if (s.startsWith("_")){
            return s.substring(1);
        }
        return s;
    }
}
