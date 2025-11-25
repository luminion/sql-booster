package io.github.luminion.sqlbooster.model.api;

import lombok.*;

/**
 * SQL 排序实体类.
 * <p>
 * 实现了 {@link Sort} 接口, 用于表示 SQL 查询中的排序规则.
 *
 * @author luminion
 * @since 1.0.0
 */

@Getter
@ToString
@EqualsAndHashCode
@NoArgsConstructor
@AllArgsConstructor
public class Sort {

    /**
     * 排序字段.
     */
    protected String field;

    /**
     * 是否为升序排列.
     */
    protected boolean asc;

    /**
     * 从 {@link Sort} 实例创建 {@link Sort} 实例.
     *
     * @param sort SQL 排序接口实例
     * @return {@link Sort} SQL 排序实体实例
     * @since 1.0.0
     */
    public static Sort of(Sort sort) {
        return new Sort(sort.getField(), sort.isAsc());
    }

}