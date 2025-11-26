package io.github.luminion.sqlbooster.model.sql;

import io.github.luminion.sqlbooster.model.api.Wrapper;
import io.github.luminion.sqlbooster.model.api.Sort;
import io.github.luminion.sqlbooster.model.api.Tree;
import lombok.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * 可排序的条件树实体类.
 * <p>
 * 扩展 {@link SqlTree} 类, 增加了排序功能, 用于表示包含排序信息的 SQL 条件树.
 *
 * @param <T> 实体类型
 * @author luminion
 * @since 1.0.0
 */
@Getter
@ToString
@EqualsAndHashCode(callSuper = true)
public class SqlWrapper<T> extends SqlTree implements Wrapper<T> {

    /**
     * 排序字段列表.
     */
    protected Collection<Sort> sorts;
    
    /**
     * 非本表字段的额外条件.
     */
    protected transient Map<String, Object> extra;

    {
        this.sorts = new LinkedHashSet<>();
        this.extra = new HashMap<>();
    }

    /**
     * {@inheritDoc}
     * @since 1.0.0
     */
    @Override
    @SuppressWarnings({"rawtypes", "unchecked"})
    protected SqlTree addChild(Tree sqlTree) {
        if (sqlTree==null){
            return this;
        }
        if (sqlTree instanceof SqlWrapper){
            SqlWrapper sqlWrapper = (SqlWrapper) sqlTree;
            this.sorts.addAll(sqlWrapper.getSorts());
        }
        return super.addChild(sqlTree);
    }
}