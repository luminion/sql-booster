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
@Data
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

}