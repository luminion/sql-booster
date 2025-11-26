package io.github.luminion.sqlbooster.model.api;

/**
 * 条件接口.
 * <p>
 * 查询条件的基本结构, 包括字段名、操作符和值.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface Condition {

    /**
     * 获取条件的字段名.
     *
     * @return 字段名
     * @since 1.0.0
     */
    String getField();

    /**
     * 获取条件的操作符 (例如 =, LIKE, IN).
     *
     * @return 操作符
     * @since 1.0.0
     */
    String getOperator();

    /**
     * 获取条件的值.
     *
     * @return 条件值
     * @since 1.0.0
     */
    Object getValue();

}