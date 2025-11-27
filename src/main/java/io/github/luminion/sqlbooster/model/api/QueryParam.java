package io.github.luminion.sqlbooster.model.api;

import io.github.luminion.sqlbooster.core.BoosterParam;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 查询参数封装类
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class QueryParam<T> extends ConditionNode implements BoosterParam<T> {

    /**
     * 排序字段列表.
     */
    protected LinkedHashSet<Sort> sorts = new LinkedHashSet<>();

    /**
     * 非本表字段的额外条件.
     */
    protected transient Map<String, Object> extra = new HashMap<>();
    
    @Override
    protected ConditionNode merge(ConditionNode segment) {
        if (segment == null) {
            return this;
        }
        if (segment instanceof QueryParam) {
            QueryParam<?> queryParams = (QueryParam<?>) segment;
            this.sorts.addAll(queryParams.getSorts());
        }
        return super.merge(segment);
    }
}