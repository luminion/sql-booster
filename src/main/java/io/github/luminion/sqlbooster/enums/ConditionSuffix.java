package io.github.luminion.sqlbooster.enums;

import io.github.luminion.sqlbooster.util.StrUtils;
import lombok.Getter;

/**
 * 查询字段后缀与 SQL 操作符的映射枚举.
 * <p>
 * 自动支持两种风格：
 * 1. 驼峰风格 (e.g., "NameEq", "AgeGt")
 * 2. 下划线风格 (e.g., "_name_eq", "_age_gt")
 * </p>
 *
 * @author luminion
 * @since 1.0.0
 */
public enum ConditionSuffix {
    NE(SqlKeyword.NE, "Ne"),
    LT(SqlKeyword.LT, "Lt"),
    LTE(SqlKeyword.LTE, "Lte"),
    GT(SqlKeyword.GT, "Gt"),
    GTE(SqlKeyword.GTE, "Gte"),

    LIKE(SqlKeyword.LIKE, "Like"),
    NOT_LIKE(SqlKeyword.NOT_LIKE, "NotLike"),

    IN(SqlKeyword.IN, "In"),
    NOT_IN(SqlKeyword.NOT_IN, "NotIn"),

    IS_NULL(SqlKeyword.IS_NULL, "IsNull"),
    IS_NOT_NULL(SqlKeyword.IS_NOT_NULL, "IsNotNull"),

    BIT_ANY(SqlKeyword.BIT_ANY, "BitAny"),
    BIT_ALL(SqlKeyword.BIT_ALL, "BitAll"),
    BIT_NONE(SqlKeyword.BIT_NONE, "BitNone");

    /**
     * 对应的 SQL 关键字.
     */
    private final SqlKeyword sqlKeyword;
    /**
     * 标准驼峰后缀 (e.g., "Ne", "NotLike").
     */
    @Getter
    private final String camelCase;
    /**
     * 自动生成的下划线后缀 (e.g., "_ne", "_not_like").
     */
    @Getter
    private final String underscore;

    ConditionSuffix(SqlKeyword sqlKeyword, String camelCase) {
        this.camelCase = camelCase;
        this.sqlKeyword = sqlKeyword;
        // 自动生成下划线风格: "NotLike" -> "_not_like"
        this.underscore = StrUtils.camelCaseToUnderscore(camelCase);
    }

    public String getKeyword() {
        return sqlKeyword.getSymbol();
    }
}