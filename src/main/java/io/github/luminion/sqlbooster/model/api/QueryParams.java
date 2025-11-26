package io.github.luminion.sqlbooster.model.api;

import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 可排序的条件树实体类.
 * <p>
 * 扩展 {@link QuerySegment} 类, 增加了排序功能, 用于表示包含排序信息的 SQL 条件树.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */@Data
@EqualsAndHashCode(callSuper = true)
public class QueryParams<T> extends QuerySegment {

    /**
     * 排序字段列表.
     */
    protected LinkedHashSet<Sort> sorts = new LinkedHashSet<>();

    /**
     * 非本表字段的额外条件.
     */
    protected transient Map<String, Object> extra = new HashMap<>();
    
    @Override
    protected QuerySegment appendTree(QuerySegment segment) {
        if (segment == null) {
            return this;
        }
        if (segment instanceof QueryParams) {
            QueryParams<?> queryParams = (QueryParams<?>) segment;
            this.sorts.addAll(queryParams.getSorts());
        }
        return super.appendTree(segment);
    }
}