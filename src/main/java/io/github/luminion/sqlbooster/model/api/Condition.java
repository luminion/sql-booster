package io.github.luminion.sqlbooster.model.api;

import io.github.luminion.sqlbooster.model.enums.SqlKeyword;
import lombok.*;

/**
 * SQL 条件实体类.
 * <p>
 * 实现了 {@link Condition} 接口, 用于表示 SQL 查询中的单个条件, 包括字段名、操作符和值.
 *
 * @author luminion
 * @since 1.0.0
 */
@Getter
@ToString
@EqualsAndHashCode
@NoArgsConstructor
public class Condition {

    /**
     * 字段名.
     */
    protected String field;
    /**
     * 操作符.
     */
    protected String operator;
    /**
     * 条件值.
     */
    protected Object value;

    {
        this.operator = SqlKeyword.EQ.getKeyword();
        this.value = "";
    }

    /**
     * 使用字段名和值构造一个默认操作符为等于 (EQ) 的条件.
     *
     * @param field 字段名
     * @param value 条件值
     * @since 1.0.0
     */
    public Condition(String field, Object value) {
        this.field = field;
        this.value = value;
    }

    /**
     * 使用字段名、操作符和值构造一个条件.
     *
     * @param field    字段名
     * @param operator 操作符
     * @param value    条件值
     * @since 1.0.0
     */
    public Condition(String field, String operator, Object value) {
        this.field = field;
        this.operator = SqlKeyword.replaceOperator(operator);
        this.value = value;
    }

    /**
     * 从 {@link Condition} 实例创建 {@link Condition} 实例.
     *
     * @param sqlCondition SQL 条件接口实例
     * @return {@link Condition} SQL 条件实体实例
     * @since 1.0.0
     */
    public static Condition of(Condition sqlCondition) {
        return new Condition(sqlCondition.getField(), sqlCondition.getOperator(), sqlCondition.getValue());
    }


}