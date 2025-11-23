import React, { useState, useEffect } from 'react';
import {
  Box,
  Card,
  CardContent,
  Typography,
  Alert,
  CircularProgress,
  Grid,
  List,
  ListItem,
  ListItemText,
  Chip,
  Divider,
  Accordion,
  AccordionSummary,
  AccordionDetails,
} from '@mui/material';
import {
  ExpandMore as ExpandMoreIcon,
  CallMade as CallMadeIcon,
  CallReceived as CallReceivedIcon,
  Storage as StorageIcon,
} from '@mui/icons-material';
import api from '../../services/api';

interface ProgramDependencyViewProps {
  uploadId: string;
  programName: string;
}

const ProgramDependencyView: React.FC<ProgramDependencyViewProps> = ({
  uploadId,
  programName,
}) => {
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [callers, setCallers] = useState<any[]>([]);
  const [callees, setCallees] = useState<any[]>([]);
  const [dbUsage, setDbUsage] = useState<any[]>([]);

  useEffect(() => {
    loadDependencies();
  }, [uploadId, programName]);

  const loadDependencies = async () => {
    setLoading(true);
    setError(null);

    try {
      const [callersData, calleesData, dbUsageData] = await Promise.all([
        api.getProgramCallers(uploadId, programName),
        api.getProgramCallees(uploadId, programName),
        api.getDatabaseUsage(uploadId, programName),
      ]);

      setCallers(callersData.callers);
      setCallees(calleesData.callees);
      setDbUsage(dbUsageData.database_usage);

      setLoading(false);
    } catch (err: any) {
      setError(err.message || 'Failed to load dependencies');
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" p={4}>
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Alert severity="error" sx={{ mb: 2 }}>
        {error}
      </Alert>
    );
  }

  return (
    <Grid container spacing={3}>
      {/* Callers */}
      <Grid item xs={12} md={6}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              <CallReceivedIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
              Called By ({callers.length})
            </Typography>
            <Divider sx={{ mb: 2 }} />
            {callers.length === 0 ? (
              <Typography variant="body2" color="textSecondary">
                No callers found (this might be an entry point)
              </Typography>
            ) : (
              <List dense>
                {callers.map((caller: any, idx: number) => (
                  <ListItem key={idx}>
                    <ListItemText
                      primary={caller.caller}
                      secondary={
                        <>
                          <Typography variant="caption" display="block">
                            Line {caller.caller_line_number} • {caller.call_type}
                          </Typography>
                          <Typography variant="caption" color="textSecondary">
                            {caller.call_signature}
                          </Typography>
                        </>
                      }
                    />
                    <Chip
                      label={caller.call_mechanism}
                      size="small"
                      variant="outlined"
                    />
                  </ListItem>
                ))}
              </List>
            )}
          </CardContent>
        </Card>
      </Grid>

      {/* Callees */}
      <Grid item xs={12} md={6}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              <CallMadeIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
              Calls ({callees.length})
            </Typography>
            <Divider sx={{ mb: 2 }} />
            {callees.length === 0 ? (
              <Typography variant="body2" color="textSecondary">
                No outgoing calls found
              </Typography>
            ) : (
              <List dense>
                {callees.map((callee: any, idx: number) => (
                  <ListItem key={idx}>
                    <ListItemText
                      primary={
                        <Box display="flex" alignItems="center" gap={1}>
                          {callee.callee}
                          {callee.is_external === 1 && (
                            <Chip
                              label="External"
                              size="small"
                              color="warning"
                            />
                          )}
                        </Box>
                      }
                      secondary={
                        <>
                          <Typography variant="caption" display="block">
                            Line {callee.caller_line_number} • {callee.call_type}
                          </Typography>
                          <Typography variant="caption" color="textSecondary">
                            {callee.call_signature}
                          </Typography>
                        </>
                      }
                    />
                    <Chip
                      label={callee.call_mechanism}
                      size="small"
                      variant="outlined"
                    />
                  </ListItem>
                ))}
              </List>
            )}
          </CardContent>
        </Card>
      </Grid>

      {/* Database Usage */}
      <Grid item xs={12}>
        <Card>
          <CardContent>
            <Typography variant="h6" gutterBottom>
              <StorageIcon sx={{ mr: 1, verticalAlign: 'middle' }} />
              Database Objects Used ({dbUsage.length})
            </Typography>
            <Divider sx={{ mb: 2 }} />
            {dbUsage.length === 0 ? (
              <Typography variant="body2" color="textSecondary">
                No database access found
              </Typography>
            ) : (
              dbUsage.map((usage: any, idx: number) => (
                <Accordion key={idx}>
                  <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                    <Box display="flex" alignItems="center" gap={2} width="100%">
                      <Typography sx={{ flexGrow: 1 }}>
                        {usage.db_object_name}
                      </Typography>
                      <Chip
                        label={usage.access_type}
                        size="small"
                        color={usage.access_mode === 'WRITE' ? 'error' : 'info'}
                      />
                      <Chip
                        label={`${usage.access_count} times`}
                        size="small"
                        variant="outlined"
                      />
                    </Box>
                  </AccordionSummary>
                  <AccordionDetails>
                    <Box>
                      <Typography variant="body2" gutterBottom>
                        <strong>Type:</strong> {usage.object_type || 'TABLE'}
                      </Typography>
                      <Typography variant="body2" gutterBottom>
                        <strong>Database:</strong> {usage.database_type || 'Unknown'}
                      </Typography>
                      {usage.columns && (
                        <Typography variant="body2" gutterBottom>
                          <strong>Columns:</strong> {usage.columns}
                        </Typography>
                      )}
                      {usage.line_numbers && usage.line_numbers.length > 0 && (
                        <Typography variant="body2">
                          <strong>Lines:</strong> {usage.line_numbers.join(', ')}
                        </Typography>
                      )}
                    </Box>
                  </AccordionDetails>
                </Accordion>
              ))
            )}
          </CardContent>
        </Card>
      </Grid>
    </Grid>
  );
};

export default ProgramDependencyView;
