package io.github.luminion.sqlbooster.extension.pagehelper;

import com.github.pagehelper.PageInfo;
import io.github.luminion.sqlbooster.model.BPage;
import io.github.luminion.sqlbooster.util.ReflectUtils;
import lombok.RequiredArgsConstructor;

import java.util.List;

/**
 * 将 PageHelper 的 {@link PageInfo} 适配为 {@link BPage} 的包装类。
 *
 * @param <T> 记录的类型
 */
@RequiredArgsConstructor
public class PhPage<T> implements BPage<T> {
    private static final long serialVersionUID = 1L;

    private final PageInfo<T> pageInfo;

    @Override
    public List<T> getRecords() {
        return pageInfo.getList();
    }

    @Override
    public long getTotal() {
        return pageInfo.getTotal();
    }

    @Override
    public long getCurrent() {
        return pageInfo.getPageNum();
    }

    @Override
    public long getSize() {
        return pageInfo.getPageSize();
    }

    @Override
    public <R> BPage<R> convertRecords(Class<R> targetType) {
        PageInfo<R> convert = pageInfo.convert(e -> ReflectUtils.toTarget(e, targetType));
        return new PhPage<>(convert);
    }
}
