package io.github.luminion.sqlbooster.util;

/**
 * @author luminion
 * @since 1.0.0
 */
public class StrUtils {

    /**
     * 将下划线命名的字符串转换为驼峰命名.
     *
     * @param str 待转换的字符串
     * @return 驼峰命名的字符串
     * @since 1.0.0
     */
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

    /**
     * 将驼峰命名的字符串转换为下划线命名.
     *
     * @param str 待转换的字符串
     * @return 下划线命名的字符串
     * @since 1.0.0
     */
    public static String camelCaseToUnderscore(String str) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (Character.isUpperCase(c)) {
                sb.append('_').append(Character.toLowerCase(c));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * 将驼峰首字母大写的命名的字符串转换为下划线命名.
     *
     * @param str 待转换的字符串
     * @return 下划线命名的字符串
     * @since 1.0.0
     */
    public static String pascalCaseToUnderscore(String str) {
        String s = camelCaseToUnderscore(str);
        if (s.startsWith("_")){
            return s.substring(1);
        }
        return s;
    }
    
    
}
