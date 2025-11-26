package io.github.luminion.sqlbooster.model.api;

/**
 * 排序接口.
 * <p>
 * 定义查询中的排序规则, 包括排序字段和排序方向.
 *
 * @author luminion
 * @since 1.0.0
 */
public interface Sort {

    /**
     * 获取排序字段的属性名.
     *
     * @return 属性名
     * @since 1.0.0
     */
    String getField();

    /**
     * 是否为升序排列
     *
     * @return true 表示升序, false 表示降序
     * @since 1.0.0
     */
    boolean isAsc();

}