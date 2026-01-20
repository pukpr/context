# ClioPatria Context Server - Deployment Guide

## Quick Start

### Prerequisites

1. **Install SWI-Prolog**:
   ```bash
   sudo apt-get update
   sudo apt-get install swi-prolog
   ```

2. **Install Graphviz** (for datacloud visualization):
   ```bash
   sudo apt-get install graphviz
   ```

3. **Verify Installation**:
   ```bash
   swipl --version
   # Should show: SWI-Prolog version 9.0.4 or later
   
   dot -V
   # Should show: dot - graphviz version 2.43.0 or later
   ```

### Basic Deployment (Core Functionality Only)

The server will run with basic RDF/Semantic Web features but **without R integration**:

```bash
cd dynamic_context_server
./run-cloud.sh start
```

Access the server at: **http://localhost:3020/**

**Note**: The datacloud visualization feature requires graphviz. See Prerequisites above.

### Full Deployment (With R Integration)

For complete climate/geophysical modeling capabilities:

1. **Install R**:
   ```bash
   sudo apt-get install r-base r-base-dev
   ```

2. **Install Required R Packages**:
   ```r
   R
   > install.packages(c("gsl", "graphics"))
   > q()
   ```

3. **Install SWI-Prolog Real Pack**:
   ```bash
   swipl -g "pack_install(real, [interactive(false)])" -t halt
   ```

4. **Start Server**:
   ```bash
   cd dynamic_context_server
   ./run-cloud.sh start
   ```

---

## Server Management

### Start Server
```bash
./run-cloud.sh start
```

### Stop Server
```bash
./run-cloud.sh stop
```

### Restart Server
```bash
./run-cloud.sh restart
```

### Check Status
```bash
./run-cloud.sh status
```

### View Logs
```bash
tail -f server.log
```

---

## Legacy Script (Deprecated)

The old `nohup-run-cloud` script is **deprecated**. Use `run-cloud.sh` instead.

**Why?**
- Old script had incorrect syntax (`cp_server,sleep(100000000)`)
- No proper process management
- No logging control
- Requires sudo unnecessarily

---

## Configuration

### Port Configuration

Default port is **3020**. To change:

1. Edit `config-enabled/network.pl`:
   ```prolog
   :- set_setting_default(http:port, 8080).  % Uncomment and change
   ```

2. Restart server:
   ```bash
   ./run-cloud.sh restart
   ```

### Public Access Configuration

To make server accessible from other machines:

1. Edit `config-enabled/localhost.pl` or create custom config
2. Set public host/port:
   ```prolog
   :- set_setting_default(http:public_host, 'your-domain.com').
   :- set_setting_default(http:public_port, 80).
   ```

---

## Production Deployment

### Using Systemd (Recommended)

Create `/etc/systemd/system/cliopatria-context.service`:

```ini
[Unit]
Description=ClioPatria Context Server
After=network.target

[Service]
Type=forking
User=www-data
Group=www-data
WorkingDirectory=/path/to/context/dynamic_context_server
ExecStart=/path/to/context/dynamic_context_server/run-cloud.sh start
ExecStop=/path/to/context/dynamic_context_server/run-cloud.sh stop
Restart=on-failure
RestartSec=10
PIDFile=/path/to/context/dynamic_context_server/server.pid

[Install]
WantedBy=multi-user.target
```

Enable and start:
```bash
sudo systemctl daemon-reload
sudo systemctl enable cliopatria-context
sudo systemctl start cliopatria-context
sudo systemctl status cliopatria-context
```

### Reverse Proxy with Nginx

```nginx
server {
    listen 80;
    server_name your-domain.com;

    location / {
        proxy_pass http://localhost:3020;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

---

## Firewall Configuration

### Allow Port 3020
```bash
sudo ufw allow 3020/tcp
```

### For Nginx Reverse Proxy
```bash
sudo ufw allow 'Nginx Full'
```

---

## Troubleshooting

### Server Won't Start

1. **Check logs**:
   ```bash
   tail -100 server.log
   ```

2. **Common issues**:
   - Port 3020 already in use: `sudo lsof -i :3020`
   - Permissions issue: Check file ownership
   - Missing dependencies: Install SWI-Prolog

### R Integration Not Working

**Symptoms**: Errors like `source_sink 'library(real)' does not exist`

**Solution**:
1. Install R: `sudo apt-get install r-base r-base-dev`
2. Install real pack: `swipl -g "pack_install(real)" -t halt`
3. Restart server

### Import Conflicts Warning

**Warning**: `No permission to import X into user (already imported from Y)`

**Impact**: Low - First module wins, functionality should work
**Solution**: Not critical, but can fix by using qualified module calls

### Server Running But Not Accessible

1. **Check server status**:
   ```bash
   ./run-cloud.sh status
   ```

2. **Check port**:
   ```bash
   netstat -tlnp | grep 3020
   ```

3. **Check firewall**:
   ```bash
   sudo ufw status
   ```

4. **Test locally**:
   ```bash
   curl http://localhost:3020/
   ```

---

## Known Limitations

### Without Graphviz:
- ❌ Datacloud visualization feature won't work
- ✅ All other RDF/SPARQL features work

### Without R Integration:
- ❌ Statistical plotting functions
- ❌ Bessel function calculations  
- ❌ R-based regression analysis
- ❌ Advanced statistical modeling
- ✅ RDF/SPARQL queries work
- ✅ Web interface works
- ✅ Data browsing works

### With Graphviz + R Integration:
- ✅ Full functionality
- ✅ All modeling features
- ✅ Plotting and visualization
- ✅ Datacloud visualization

---

## Security Considerations

### Production Checklist:

- [ ] Change default passwords (if any)
- [ ] Configure HTTPS (use nginx with Let's Encrypt)
- [ ] Restrict network access (firewall rules)
- [ ] Run as non-root user (www-data)
- [ ] Keep SWI-Prolog updated
- [ ] Monitor logs regularly
- [ ] Set up log rotation
- [ ] Configure backup strategy

### Recommended Security Settings:

1. **Disable directory listing** (in ClioPatria config)
2. **Enable access control** (for sensitive endpoints)
3. **Use HTTPS** (for production)
4. **Regular updates** (check for ClioPatria updates)

---

## Performance Tuning

### For Large Datasets:

Edit `config-enabled/network.pl`:
```prolog
% Increase worker threads
:- set_setting_default(http:workers, 16).
```

### Memory Settings:

For custom memory settings, modify `run-cloud.sh` to include SWI-Prolog flags:

```bash
# In run-cloud.sh, change the nohup line to:
nohup swipl --stack-limit=2G -g '(cp_server, thread_get_message(keep_alive))' -s run.pl > "$LOG_FILE" 2>&1 &
```

Or start manually with more stack space:
```bash
swipl --stack-limit=2G -g '(cp_server, thread_get_message(keep_alive))' -s run.pl
```

---

## Monitoring

### Check Server Health:
```bash
curl http://localhost:3020/
```

### Monitor Logs:
```bash
tail -f server.log | grep -i error
```

### Check Resource Usage:
```bash
ps aux | grep swipl
```

---

## Backup and Restore

### Backup RDF Data:
```bash
# If you've loaded RDF data
cp -r RDF-store/ RDF-store.backup.$(date +%Y%m%d)
```

### Backup Configuration:
```bash
tar czf config-backup.tar.gz config-enabled/
```

---

## Support and Documentation

- **ClioPatria Documentation**: http://cliopatria.swi-prolog.org/
- **SWI-Prolog Manual**: https://www.swi-prolog.org/pldoc/doc_for?object=manual
- **Evaluation Report**: See `../EVALUATION_REPORT.md` for detailed analysis

---

## Changelog

### 2026-01-19 - Initial Evaluation and Fixes
- ✅ Evaluated server after years of inactivity
- ✅ Created comprehensive evaluation report
- ✅ Fixed cloud deployment script
- ✅ Added proper process management
- ✅ Created deployment documentation
- ✅ Identified and documented all issues
- ✅ Server confirmed working with SWI-Prolog 9.0.4
