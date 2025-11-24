package io.github.luminion.sqlbooster.model.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

/**
 * SQL 操作符枚举.
 * <p>
 * 定义了常用的 SQL 操作符及其分类, 用于 SQL 条件构建和验证.
 *
 * @author luminion
 * @since 1.0.0
 */
@Getter
@AllArgsConstructor
public enum SqlKeyword {

    /**
     * AND 连接符
     */
    AND("AND"),
    /**
     * OR 连接符
     */
    OR("OR"),

    /**
     * 等于操作符
     */
    EQ("="),
    /**
     * 不等于操作符
     */
    NE("<>"),
    /**
     * 小于操作符
     */
    LT("<"),
    /**
     * 小于等于操作符
     */
    LTE("<="),
    /**
     * 大于操作符
     */
    GT(">"),
    /**
     * 大于等于操作符
     */
    GTE(">="),
    /**
     * 模糊匹配操作符
     */
    LIKE("LIKE"),
    /**
     * 不模糊匹配操作符
     */
    NOT_LIKE("NOT LIKE"),

    /**
     * IS NULL 操作符
     */
    IS_NULL("IS NULL"),
    /**
     * IS NOT NULL 操作符
     */
    IS_NOT_NULL("IS NOT NULL"),

    /**
     * IN 操作符
     */
    IN("IN"),
    /**
     * NOT IN 操作符
     */
    NOT_IN("NOT IN"),

    /**
     * 包含任意指定bit位
     */
    BIT_ANY("BIT ANY"),
    /**
     * 包含所有指定bit位
     */
    BIT_ALL("BIT ALL"),
    /**
     * 不包含指定bit位
     */
    BIT_NONE("BIT NONE"),

    ;
    /**
     * 操作符关键字.
     */
    private final String keyword;


    /**
     * 标准化 SQL 连接符 (AND/OR).
     *
     * @param connector 连接符字符串
     * @return 标准化后的连接符 (大写)
     * @throws IllegalArgumentException 当连接符不是 AND 或 OR 时抛出
     * @since 1.0.0
     */
    public static String replaceConnector(String connector) {
        if (connector == null || connector.isEmpty()) {
            return SqlKeyword.AND.getKeyword();
        }
        connector = connector.toUpperCase();
        if (SqlKeyword.AND.getKeyword().equals(connector) || SqlKeyword.OR.getKeyword().equals(connector)) {
            return connector;
        }
        throw new IllegalArgumentException("illegal operator: " + connector);
    }

    /**
     * 标准化 SQL 操作符.
     *
     * @param operator 操作符字符串
     * @return 标准化后的操作符
     * @throws IllegalArgumentException 当操作符不支持时抛出
     * @since 1.0.0
     */
    public static String replaceOperator(String operator) {
        if (operator == null || operator.isEmpty()) {
            return SqlKeyword.EQ.getKeyword();
        }
        operator = operator.toUpperCase();
        switch (operator) {
            case "=":
            case "==":
            case "EQ":
                return SqlKeyword.EQ.getKeyword();
            case "<>":
            case "!=":
            case "NE":
                return SqlKeyword.NE.getKeyword();
            case "<":
            case "LT":
                return SqlKeyword.LT.getKeyword();
            case "<=":
            case "LE":
            case "LTE":
                return SqlKeyword.LTE.getKeyword();
            case ">":
            case "GT":
                return SqlKeyword.GT.getKeyword();
            case ">=":
            case "GE":
            case "GTE":
                return SqlKeyword.GTE.getKeyword();
            case "LIKE":
                return SqlKeyword.LIKE.getKeyword();
            case "NOT LIKE":
            case "NOT_LIKE":
                return SqlKeyword.NOT_LIKE.getKeyword();
            case "IN":
                return SqlKeyword.IN.getKeyword();
            case "NOT IN":
            case "NOT_IN":
                return SqlKeyword.NOT_IN.getKeyword();
            case "IS NULL":
            case "IS_NULL":
                return SqlKeyword.IS_NULL.getKeyword();
            case "IS NOT NULL":
            case "IS_NOT_NULL":
                return SqlKeyword.IS_NOT_NULL.getKeyword();
            case "BIT ANY":
            case "BIT_ANY":
                return SqlKeyword.BIT_ANY.getKeyword();
            case "BIT ALL":
            case "BIT_ALL":
                return SqlKeyword.BIT_ALL.getKeyword();
            case "BIT NONE":
            case "BIT_NONE":
                return SqlKeyword.BIT_NONE.getKeyword();
            default:
                throw new IllegalArgumentException("illegal operator: " + operator);
        }
    }


    /**
     * 判断是否为相等或不相等操作符.
     *
     * @param operator 操作符
     * @return 如果是相等或不相等操作符返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean isEqOperator(String operator) {
        return SqlKeyword.EQ.getKeyword().equals(operator) || SqlKeyword.NE.getKeyword().equals(operator);
    }

    /**
     * 判断是否为比较大小的操作符 (不包含等于和不等于).
     *
     * @param operator 操作符
     * @return 如果是比较大小的操作符返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean isCompareOperator(String operator) {
        return SqlKeyword.LT.getKeyword().equals(operator) || SqlKeyword.LTE.getKeyword().equals(operator) ||
                SqlKeyword.GT.getKeyword().equals(operator) || SqlKeyword.GTE.getKeyword().equals(operator);
    }

    /**
     * 判断是否为 LIKE 或 NOT LIKE 操作符.
     *
     * @param operator 操作符
     * @return 如果是 LIKE 或 NOT LIKE 操作符返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean isLikeOperator(String operator) {
        return SqlKeyword.LIKE.getKeyword().equals(operator) || SqlKeyword.NOT_LIKE.getKeyword().equals(operator);
    }

    /**
     * 判断是否为 IN 或 NOT IN 操作符.
     *
     * @param operator 操作符
     * @return 如果是 IN 或 NOT IN 操作符返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean isInOperator(String operator) {
        return SqlKeyword.IN.getKeyword().equals(operator) || SqlKeyword.NOT_IN.getKeyword().equals(operator);
    }

    /**
     * 判断是否为 IS NULL 或 IS NOT NULL 操作符.
     *
     * @param operator 操作符
     * @return 如果是 IS NULL 或 IS NOT NULL 操作符返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean isNullOperator(String operator) {
        return SqlKeyword.IS_NULL.getKeyword().equals(operator) || SqlKeyword.IS_NOT_NULL.getKeyword().equals(operator);
    }

    /**
     * 判断是否为位操作符.
     *
     * @param operator 操作符
     * @return 如果是位操作符返回 true, 否则返回 false
     * @since 1.0.0
     */
    public static boolean isBitOperator(String operator) {
        return SqlKeyword.BIT_ANY.getKeyword().equals(operator) || SqlKeyword.BIT_ALL.getKeyword().equals(operator)|| SqlKeyword.BIT_NONE.getKeyword().equals(operator);
    }

}